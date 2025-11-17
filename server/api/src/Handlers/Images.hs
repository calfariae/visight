{-# LANGUAGE OverloadedStrings #-}

module Handlers.Images where

import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status
import Web.Scotty
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session

import Models.Image
import Database.Statements

-- | GET /images - Get all images
getAllImagesHandler :: Connection.Connection -> ActionM ()
getAllImagesHandler conn = do
  result <- liftIO $ Session.run (Session.statement () getAllImagesStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right images -> json images

-- | GET /users/:userId/images - Get images by user ID
getImagesByUserIdHandler :: Connection.Connection -> ActionM ()
getImagesByUserIdHandler conn = do
  uid <- pathParam "userId"
  result <- liftIO $ Session.run (Session.statement uid getImagesByUserIdStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right images -> json images

-- | POST /images - Create a new image
createImageHandler :: Connection.Connection -> ActionM ()
createImageHandler conn = do
  createImage <- jsonData :: ActionM CreateImage
  result <- liftIO $ Session.run (Session.statement (ciUserId createImage) createImageStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right image -> do
      status status201
      json image

-- | DELETE /images/:id - Delete image
deleteImageHandler :: Connection.Connection -> ActionM ()
deleteImageHandler conn = do
  iid <- pathParam "id"
  result <- liftIO $ Session.run (Session.statement iid deleteImageStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right rowsAffected ->
      if rowsAffected > 0
        then do
          status status204
          json $ object ["message" .= ("Image deleted" :: String)]
        else do
          status status404
          json $ object ["error" .= ("Image not found" :: String)]