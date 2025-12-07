{-# LANGUAGE OverloadedStrings #-}

module Handlers.Images where

import Control.Monad (when)
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status
import Web.Scotty
import Models.Image
import Database.Statements
import System.Directory (createDirectoryIfMissing)
import Network.Wai.Parse (fileContent)
import System.FilePath ((</>))
import System.Directory (doesFileExist, removeFile)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A

-- | GET /images - Get all images
getAllImagesHandler :: Connection.Connection -> ActionM ()
getAllImagesHandler conn = do
  result <- liftIO $ Session.run (Session.statement () getAllImagesStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right images -> json images

-- | GET /users/:userId/images - Get image by ID
getImageByIdHandler :: Connection.Connection -> ActionM ()
getImageByIdHandler conn = do
  iid <- pathParam "id"
  result <- liftIO $ Session.run (Session.statement iid getImageByIdStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right Nothing -> do
      status status404
      json $ object ["error" .= ("Image not found" :: String)]
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

-- | PUT /users/:id - Update user
updateImageHandler :: Connection.Connection -> ActionM ()
updateImageHandler conn = do
  iid <- pathParam "id"
  updateImage <- jsonData :: ActionM UpdateImage
  let imageParams = (iid, uiImageWeight updateImage)
  result <- liftIO $ Session.run (Session.statement imageParams updateImageStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right Nothing -> do
      status status404
      json $ object ["error" .= ("User not found" :: String)]
    Right (Just image) -> json image

-- | DELETE /images/:id - Delete image
deleteImageHandler :: Connection.Connection -> ActionM ()
deleteImageHandler conn = do
  iid <- pathParam "id"
  
  -- First, get the image to retrieve file path
  result <- liftIO $ Session.run (Session.statement iid getImageByIdStatement) conn
  
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right image -> do
      case image of
        Nothing -> do
          status status404
          json $ object ["error" .= ("Image not found" :: String)]
        Just image -> do
          -- Delete the physical file
          let filePath = T.unpack $ imagePath image  -- Add T.unpack here
          fileExists <- liftIO $ doesFileExist filePath
          when fileExists $ liftIO $ removeFile filePath
          
          -- Delete from database
          result <- liftIO $ Session.run (Session.statement iid deleteImageStatement) conn
          case result of
            Left err -> do
              status status500
              json $ object ["error" .= ("Database error" :: String), "details" .= show err]
            Right _ -> do
              status status204
              json $ object ["message" .= ("Image deleted" :: String)]

  -- as it stands right now, anyone can just request to the url
  -- given a uid that doesn't exist, it'll still create a directory for the nonexistant user
  -- given an iid that already exists, it'll rewrite the already existing image
  -- or create a new one if given a different uid

uploadImageHandler :: Connection.Connection -> ActionM ()
uploadImageHandler conn = do
  iid <- pathParam "id" :: ActionM Int
  uid <- formParam "userId" :: ActionM TL.Text
  let userId = read (TL.unpack uid) :: Int
  fs <- files
  case fs of
    [] -> do
      status status400
      json $ object ["error" .= ("No image file uploaded" :: String)]
    ((_, fileInfo):_) -> do
      let content = fileContent fileInfo
      let userDir = "../uploads" </> show userId
      liftIO $ createDirectoryIfMissing True userDir
      let filePath = userDir </> show iid ++ ".jpg"
      liftIO $ BL.writeFile filePath content
      
      -- Make HTTP POST request with URL in body
      manager <- liftIO $ newManager tlsManagerSettings
      let imageUrl = "http://api:8080/images/" ++ show iid ++ "/file" -- "http://localhost:8080 if not on docker"
      initialRequest <- liftIO $ parseRequest "http://ml:5000/predict" -- "http://localhost:5000 if not on docker"
      let request = initialRequest 
            { method = "POST"
            , requestBody = RequestBodyLBS $ A.encode $ object 
                [ "url" .= imageUrl
                , "imageId" .= iid
                ]
            , requestHeaders = [("Content-Type", "application/json")]
            }
      response <- liftIO $ httpLbs request manager
      let predictionResponse = A.decode (responseBody response) :: Maybe A.Value

      json $ object 
        ["success" .= True, "imageId" .= iid, "filePath" .= filePath, "prediction" .= predictionResponse]

-- | GET /images/:id/file - Serve the image file
serveImageHandler :: Connection.Connection -> ActionM ()
serveImageHandler conn = do
  iid <- pathParam "id"
  
  result <- liftIO $ Session.run (Session.statement iid getImageByIdStatement) conn
  
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String)]

    Right Nothing -> do
      status status404
      json $ object ["error" .= ("Image not found" :: String)]

    Right (Just image) -> do
      let filePath = T.unpack $ imagePath image  -- Convert Text to FilePath
      setHeader "Content-Type" "image/jpeg"
      file filePath