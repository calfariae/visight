import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:visight/routing/routes.dart';
import 'package:visight/views/camera_screen.dart';
import 'package:visight/views/details_screen.dart';
import 'package:visight/views/home_screen.dart';
import 'package:visight/views/recipe_creation_screen.dart';
import 'package:visight/views/signin_screen.dart';
import 'package:visight/views/signup_screen.dart';

GoRouter router() => GoRouter(
  initialLocation: Routes.signUp,
  debugLogDiagnostics: true,
  redirect: _redirect,
  routes: [
    GoRoute(
      path: Routes.home,
      builder: (context, state) {
        return const HomeScreen();
      },
    ),
    GoRoute(
      path: Routes.camera,
      builder: (context, state) {
        return const CameraScreen();
      },
    ),
    GoRoute(
      path: Routes.details,
      builder: (context, state) {
        final responseData = state.extra as Map<String, dynamic>;
        return DetailsScreen(responseData: responseData);
      },
    ),
    GoRoute(
      path: Routes.recipe,
      builder: (context, state) => const RecipeCreationScreen(),
    ),
    GoRoute(
      path: Routes.signUp,
      builder: (context, state) => const SignUpScreen(),
    ),
    GoRoute(
      path: Routes.signIn,
      builder: (context, state) => const SignInScreen(),
    ),
  ],
);

Future<String?> _redirect(BuildContext context, GoRouterState state) async {
  final prefs = await SharedPreferences.getInstance();
  final userId = prefs.getString('user_id');

  final loggedIn = userId != null && userId.isNotEmpty;
  final loggingIn =
      state.matchedLocation == Routes.signIn ||
      state.matchedLocation == Routes.signUp;

  // If user is not logged in and not on auth screens, redirect to sign up
  if (!loggedIn && !loggingIn) {
    return Routes.signUp;
  }

  // If user is logged in but on auth screens, redirect to home
  if (loggedIn && loggingIn) {
    return Routes.home;
  }

  // No redirect needed
  return null;
}