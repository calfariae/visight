import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:visight/config/config.dart';
import 'package:visight/routing/routes.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'dart:convert';

class DetailsScreen extends StatefulWidget {
  final Map<String, dynamic> responseData;

  const DetailsScreen({
    super.key,
    required this.responseData,
  });

  @override
  State<DetailsScreen> createState() => _DetailsScreenState();
}

class _DetailsScreenState extends State<DetailsScreen> {
  final TextEditingController _nameController = TextEditingController();

  @override
  void dispose() {
    _nameController.dispose();
    super.dispose();
  }

  Future<void> _saveImage() async {
    if (_nameController.text.trim().isEmpty) {
      ScaffoldMessenger.of(context).showSnackBar(
        const SnackBar(content: Text('Please enter a name for the image')),
      );
      return;
    }

    try {
      final prefs = await SharedPreferences.getInstance();
      
      // Get existing saved images
      final savedImagesJson = prefs.getString('saved_images') ?? '[]';
      final List<dynamic> savedImages = jsonDecode(savedImagesJson);
      
      // Create new image data
      final imageData = {
        'name': _nameController.text.trim(),
        'imageId': widget.responseData['imageId'],
        'totalWeight': widget.responseData['prediction']?['total_weight'] ?? 0.0,
        'detectionsCount': widget.responseData['prediction']?['detections_count'] ?? 0,
        'timestamp': DateTime.now().toIso8601String(),
      };
      
      // Add new image to the list
      savedImages.add(imageData);
      
      // Save back to SharedPreferences
      await prefs.setString('saved_images', jsonEncode(savedImages));
      
      if (mounted) {
        context.go(Routes.home);
      }
    } catch (e) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text('Error saving: $e')),
        );
      }
    }
  }

  @override
  Widget build(BuildContext context) {
    final imageId = widget.responseData['imageId'].toString();
    final totalWeight = widget.responseData['prediction']?['total_weight'] ?? 0.0;
    final imageUrl = '${Config.baseUrl}/images/$imageId/file';

    return Scaffold(
      appBar: AppBar(
        title: const Text('Image Details'),
        leading: IconButton(
          icon: const Icon(Icons.arrow_back),
          onPressed: () => context.go(Routes.home),
        ),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(24.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            // Image thumbnail
            Container(
              height: 300,
              decoration: BoxDecoration(
                color: Colors.grey[200],
                borderRadius: BorderRadius.circular(12),
              ),
              clipBehavior: Clip.antiAlias,
              child: Image.network(
                      imageUrl,
                      fit: BoxFit.cover,
                      loadingBuilder: (context, child, loadingProgress) {
                        if (loadingProgress == null) return child;
                        return Center(
                          child: CircularProgressIndicator(
                            value: loadingProgress.expectedTotalBytes != null
                                ? loadingProgress.cumulativeBytesLoaded /
                                    loadingProgress.expectedTotalBytes!
                                : null,
                          ),
                        );
                      },
                      errorBuilder: (context, error, stackTrace) {
                        return const Center(
                          child: Icon(
                            Icons.broken_image,
                            size: 64,
                            color: Colors.grey,
                          ),
                        );
                      },
                    ),
            ),
            const SizedBox(height: 32),

            // Total weight display
            Container(
              padding: const EdgeInsets.all(20),
              decoration: BoxDecoration(
                color: Colors.blue[50],
                borderRadius: BorderRadius.circular(12),
                border: Border.all(color: Colors.blue[200]!),
              ),
              child: Column(
                children: [
                  const Text(
                    'Total Weight',
                    style: TextStyle(
                      fontSize: 16,
                      fontWeight: FontWeight.w500,
                      color: Colors.black87,
                    ),
                  ),
                  const SizedBox(height: 8),
                  Text(
                    '${totalWeight.toStringAsFixed(2)} grams',
                    style: const TextStyle(
                      fontSize: 32,
                      fontWeight: FontWeight.bold,
                      color: Colors.blue,
                    ),
                  ),
                ],
              ),
            ),
            const SizedBox(height: 32),

            // Name input field
            TextField(
              controller: _nameController,
              decoration: InputDecoration(
                labelText: 'Image Name',
                hintText: 'Enter a name for this image',
                border: OutlineInputBorder(
                  borderRadius: BorderRadius.circular(12),
                ),
                prefixIcon: const Icon(Icons.edit),
              ),
              textInputAction: TextInputAction.done,
              onSubmitted: (_) => _saveImage(),
            ),
            const SizedBox(height: 24),

            // Save button
            ElevatedButton(
              onPressed: _saveImage,
              style: ElevatedButton.styleFrom(
                padding: const EdgeInsets.symmetric(vertical: 16),
                shape: RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(12),
                ),
              ),
              child: const Text(
                'Save',
                style: TextStyle(fontSize: 16),
              ),
            ),
          ],
        ),
      ),
    );
  }
}