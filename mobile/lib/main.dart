import 'package:camera/camera.dart';
import 'package:flutter/material.dart';
import 'package:visight/routing/router.dart';

Future <void> main() async {
  WidgetsFlutterBinding.ensureInitialized();
  final cameras = await availableCameras();
  runApp(const MainApp());
}

class MainApp extends StatelessWidget {
  const MainApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp.router(
      routerConfig: router()
    );
  }
}
