import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:visight/config/config.dart';
import 'package:visight/routing/routes.dart';
import 'dart:convert';

import 'package:visight/views/widgets/image_card.dart';


class RecipeCreationScreen extends StatefulWidget {
  const RecipeCreationScreen({super.key});

  @override
  State<RecipeCreationScreen> createState() => _RecipeCreationScreenState();
}

class _RecipeCreationScreenState extends State<RecipeCreationScreen> {
  final TextEditingController _nameController = TextEditingController();
  final Set<int> _selectedIndices = {};
  List<Map<String, dynamic>> _savedImages = [];
  bool _isLoading = true;

  @override
  void initState() {
    super.initState();
    _loadSavedImages();
  }

  @override
  void dispose() {
    _nameController.dispose();
    super.dispose();
  }

  Future<void> _loadSavedImages() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      final savedImagesJson = prefs.getString('saved_images') ?? '[]';
      final List<dynamic> savedImages = jsonDecode(savedImagesJson);

      setState(() {
        _savedImages = savedImages.cast<Map<String, dynamic>>();
        _isLoading = false;
      });
    } catch (e) {
      setState(() => _isLoading = false);
      debugPrint('Error loading saved images: $e');
    }
  }

  void _toggleSelection(int index) {
    setState(() {
      if (_selectedIndices.contains(index)) {
        _selectedIndices.remove(index);
      } else {
        _selectedIndices.add(index);
      }
    });
  }

  Future<void> _saveRecipe() async {
    if (_nameController.text.trim().isEmpty) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Please enter a recipe name')),
      );
      return;
    }

    if (_selectedIndices.isEmpty) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Please select at least one ingredient')),
      );
      return;
    }

    try {
      final prefs = await SharedPreferences.getInstance();
      final recipesJson = prefs.getString('recipes') ?? '[]';
      final List<dynamic> recipes = jsonDecode(recipesJson);

      final selectedImages = _selectedIndices.map((i) => _savedImages[i]).toList();

      final recipe = {
        'name': _nameController.text.trim(),
        'images': selectedImages,
        'timestamp': DateTime.now().toIso8601String(),
      };

      recipes.add(recipe);
      await prefs.setString('recipes', jsonEncode(recipes));

      if (mounted) {
        context.go(Routes.home);
      }
    } catch (e) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text('Error saving recipe: $e')),
        );
      }
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Create Recipe'),
        actions: [
          TextButton(
            onPressed: _saveRecipe,
            child: const Text(
              'Save',
              style: TextStyle(color: Colors.black, fontSize: 16),
            ),
          ),
        ],
      ),
      body: _isLoading
          ? const Center(child: CircularProgressIndicator())
          : Column(
              children: [
                Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: TextField(
                    controller: _nameController,
                    decoration: InputDecoration(
                      labelText: 'Recipe Name',
                      hintText: 'Enter recipe name',
                      border: OutlineInputBorder(
                        borderRadius: BorderRadius.circular(12),
                      ),
                      prefixIcon: const Icon(Icons.restaurant),
                    ),
                  ),
                ),
                Padding(
                  padding: const EdgeInsets.symmetric(horizontal: 16.0),
                  child: Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Text(
                        'Select Ingredients',
                        style: Theme.of(context).textTheme.titleMedium,
                      ),
                      Text(
                        '${_selectedIndices.length} selected',
                        style: TextStyle(color: Colors.grey[600]),
                      ),
                    ],
                  ),
                ),
                const SizedBox(height: 8),
                Expanded(
                  child: _savedImages.isEmpty
                      ? Center(
                          child: Column(
                            mainAxisAlignment: MainAxisAlignment.center,
                            children: [
                              Icon(Icons.image_not_supported, size: 64, color: Colors.grey[400]),
                              const SizedBox(height: 16),
                              Text('No images available', style: TextStyle(fontSize: 18, color: Colors.grey[600])),
                            ],
                          ),
                        )
                      : GridView.builder(
                          padding: const EdgeInsets.all(16),
                          gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
                            crossAxisCount: 2,
                            crossAxisSpacing: 16,
                            mainAxisSpacing: 16,
                            childAspectRatio: 0.75,
                          ),
                          itemCount: _savedImages.length,
                          itemBuilder: (context, index) {
                            final image = _savedImages[index];
                            final isSelected = _selectedIndices.contains(index);

                            return ImageCard(
                              name: image['name'] ?? 'Untitled',
                              imageId: image['imageId'] ?? 0,
                              totalWeight: (image['totalWeight'] ?? 0.0).toDouble(),
                              detectionsCount: image['detectionsCount'] ?? 0,
                              timestamp: image['timestamp'] ?? DateTime.now().toIso8601String(),
                              baseUrl: Config.baseUrl,
                              isSelected: isSelected,
                              selectionMode: true,
                              onTap: () => _toggleSelection(index),
                            );
                          },
                        ),
                ),
              ],
            ),
    );
  }
}