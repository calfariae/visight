import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:visight/config/config.dart';
import 'dart:convert';
import 'package:visight/routing/routes.dart';
import 'package:visight/views/widgets/image_card.dart';

class HomeScreen extends StatefulWidget {
  const HomeScreen({super.key});

  @override
  State<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  int _currentIndex = 0;
  List<Map<String, dynamic>> _savedImages = [];
  List<Map<String, dynamic>> _recipes = [];
  bool _isLoading = true;

  @override
  void initState() {
    super.initState();
    _loadData();
  }

  Future<void> _loadData() async {
    setState(() => _isLoading = true);
    await Future.wait([_loadSavedImages(), _loadRecipes()]);
    setState(() => _isLoading = false);
  }

  Future<void> _loadSavedImages() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      final savedImagesJson = prefs.getString('saved_images') ?? '[]';
      final List<dynamic> savedImages = jsonDecode(savedImagesJson);
      _savedImages = savedImages.cast<Map<String, dynamic>>();
    } catch (e) {
      debugPrint('Error loading saved images: $e');
    }
  }

  Future<void> _loadRecipes() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      final recipesJson = prefs.getString('recipes') ?? '[]';
      final List<dynamic> recipes = jsonDecode(recipesJson);
      _recipes = recipes.cast<Map<String, dynamic>>();
    } catch (e) {
      debugPrint('Error loading recipes: $e');
    }
  }

  void _navigateToCreateRecipe() {
    context.go(Routes.recipe);
  }

  Future<void> _signOut() async {
    final confirmed = await showDialog<bool>(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Sign Out'),
        content: const Text('Are you sure you want to sign out?'),
        actions: [
          TextButton(
            onPressed: () => Navigator.of(context).pop(false),
            child: const Text('Cancel'),
          ),
          TextButton(
            onPressed: () => Navigator.of(context).pop(true),
            child: const Text('Sign Out'),
          ),
        ],
      ),
    );

    if (confirmed == true) {
      final prefs = await SharedPreferences.getInstance();
      await prefs.remove('user_id');
      if (mounted) {
        context.go(Routes.signIn);
      }
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text('Visight'), centerTitle: true),
      body: _isLoading
          ? const Center(child: CircularProgressIndicator())
          : _currentIndex == 0
          ? _buildImagesTab()
          : _currentIndex == 1
          ? _buildRecipesTab()
          : _buildProfileTab(),
      bottomNavigationBar: BottomNavigationBar(
        currentIndex: _currentIndex,
        onTap: (index) {
          setState(() {
            _currentIndex = index;
          });
        },
        items: const [
          BottomNavigationBarItem(icon: Icon(Icons.image), label: 'Images'),
          BottomNavigationBarItem(
            icon: Icon(Icons.restaurant_menu),
            label: 'Recipes',
          ),
          BottomNavigationBarItem(icon: Icon(Icons.person), label: 'Profile'),
        ],
      ),
      floatingActionButton: _currentIndex == 2
          ? null
          : FloatingActionButton(
              onPressed: _currentIndex == 0
                  ? () => context.go(Routes.camera)
                  : _navigateToCreateRecipe,
              child: Icon(_currentIndex == 0 ? Icons.camera_alt : Icons.add),
            ),
    );
  }

  Widget _buildImagesTab() {
    if (_savedImages.isEmpty) {
      return Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Icon(Icons.image_not_supported, size: 64, color: Colors.grey[400]),
            const SizedBox(height: 16),
            Text(
              'No saved images yet',
              style: TextStyle(fontSize: 18, color: Colors.grey[600]),
            ),
            const SizedBox(height: 8),
            Text(
              'Tap the camera button to get started',
              style: TextStyle(fontSize: 14, color: Colors.grey[500]),
            ),
          ],
        ),
      );
    }

    return RefreshIndicator(
      onRefresh: _loadData,
      child: GridView.builder(
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
          return ImageCard(
            name: image['name'] ?? 'Untitled',
            imageId: image['imageId'] ?? 0,
            totalWeight: (image['totalWeight'] ?? 0.0).toDouble(),
            detectionsCount: image['detectionsCount'] ?? 0,
            timestamp: image['timestamp'] ?? DateTime.now().toIso8601String(),
            baseUrl: Config.baseUrl,
          );
        },
      ),
    );
  }

  Widget _buildRecipesTab() {
    if (_recipes.isEmpty) {
      return Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Icon(Icons.restaurant_menu, size: 64, color: Colors.grey[400]),
            const SizedBox(height: 16),
            Text(
              'No recipes yet',
              style: TextStyle(fontSize: 18, color: Colors.grey[600]),
            ),
            const SizedBox(height: 8),
            Text(
              'Tap the + button to create a recipe',
              style: TextStyle(fontSize: 14, color: Colors.grey[500]),
            ),
          ],
        ),
      );
    }

    return RefreshIndicator(
      onRefresh: _loadData,
      child: ListView.builder(
        padding: const EdgeInsets.all(16),
        itemCount: _recipes.length,
        itemBuilder: (context, index) {
          final recipe = _recipes[index];
          final imageCount = (recipe['images'] as List).length;
          final totalWeight = (recipe['images'] as List).fold<double>(
            0.0,
            (sum, img) => sum + (img['totalWeight'] ?? 0.0),
          );

          return Card(
            margin: const EdgeInsets.only(bottom: 16),
            child: ListTile(
              leading: const CircleAvatar(child: Icon(Icons.restaurant)),
              title: Text(recipe['name'] ?? 'Untitled Recipe'),
              subtitle: Text(
                '$imageCount ingredient${imageCount != 1 ? 's' : ''} • ${totalWeight.toStringAsFixed(2)} kg total',
              ),
              trailing: const Icon(Icons.chevron_right),
              onTap: () {
                // TODO: Navigate to recipe details
              },
            ),
          );
        },
      ),
    );
  }

  Widget _buildProfileTab() {
    return Center(
      child: Padding(
        padding: const EdgeInsets.all(24.0),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const CircleAvatar(radius: 50, child: Icon(Icons.person, size: 50)),
            const SizedBox(height: 24),
            ElevatedButton.icon(
              onPressed: _signOut,
              icon: const Icon(Icons.logout),
              label: const Text('Sign Out'),
              style: ElevatedButton.styleFrom(
                padding: const EdgeInsets.symmetric(
                  horizontal: 32,
                  vertical: 16,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }
}