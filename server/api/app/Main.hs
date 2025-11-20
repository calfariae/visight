{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import System.Exit (exitFailure)
import Data.Aeson (object, (.=))

import Database.Connection
import Handlers.Users
import Handlers.Images

main :: IO ()
main = do
  -- Acquire database connection
  connResult <- acquireConnectionWithEnvFrom "../.env"
  conn <- case connResult of
    Left err -> do
      putStrLn $ "Failed to connect to database: " ++ show err
      exitFailure
    Right c -> do
      putStrLn "Database connection established"
      return c

  -- Start Scotty server
  putStrLn "Starting http://localhost:8080..."
  scotty 8080 $ do
    -- CORS middleware
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      }

    -- Health check endpoint
    get "/health" $ do
      json $ object ["status" .= ("ok" :: String)]

    -- User routes
    get "/users" $ getAllUsersHandler conn
    get "/users/:id" $ getUserByIdHandler conn
    post "/users" $ createUserHandler conn
    put "/users/:id" $ updateUserHandler conn
    delete "/users/:id" $ deleteUserHandler conn

    -- Image routes
    get "/images" $ getAllImagesHandler conn
    get "/users/:userId/images" $ getImagesByUserIdHandler conn
    post "/images" $ createImageHandler conn
    delete "/images/:id" $ deleteImageHandler conn