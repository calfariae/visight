import 'package:go_router/go_router.dart';
import 'package:visight/routing/routes.dart';
import 'package:visight/views/camera_screen.dart';
import 'package:visight/views/details_screen.dart';
import 'package:visight/views/home_screen.dart';
import 'package:visight/views/recipe_creation_screen.dart';

GoRouter router() => GoRouter(
  initialLocation: Routes.home,
  debugLogDiagnostics: true,
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
  ],
);

// Future<String?> _redirect(BuildContext context, GoRouterState state) async {
//   // if the user is not logged in, they need to login
//   final loggedIn = await context.read<AuthRepository>().isAuthenticated;
//   final loggingIn = state.matchedLocation == Routes.login;
//   if (!loggedIn) {
//     return Routes.login;
//   }

//   // if the user is logged in but still on the login page, send them to
//   // the home page
//   if (loggingIn) {
//     return Routes.home;
//   }

//   // no need to redirect at all
//   return null;
// }
