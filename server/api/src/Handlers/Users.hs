{-# LANGUAGE OverloadedStrings #-}

module Handlers.Users where

import Models.User
import Database.Statements
import Data.Aeson (object, (.=))
import Data.UUID (UUID)
import Network.HTTP.Types.Status
import Web.Scotty
import Web.Scotty (Parsable(..))
import qualified Data.UUID as UUID
import qualified Data.Text.Lazy as TL
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session

instance Parsable UUID where
  parseParam = maybe (Left "Invalid UUID") Right . UUID.fromText . TL.toStrict

-- | GET /users - Get all users
getAllUsersHandler :: Connection.Connection -> ActionM ()
getAllUsersHandler conn = do
  result <- liftIO $ Session.run (Session.statement () getAllUsersStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right users -> json $ map toPublicUser users

-- | GET /users/:id - Get user by ID
getUserByIdHandler :: Connection.Connection -> ActionM ()
getUserByIdHandler conn = do
  uid <- pathParam "id"
  result <- liftIO $ Session.run (Session.statement uid getUserByIdStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right Nothing -> do
      status status404
      json $ object ["error" .= ("User not found" :: String)]
    Right (Just user) -> json $ toPublicUser user

-- | POST /users - Create a new user
createUserHandler :: Connection.Connection -> ActionM ()
createUserHandler conn = do
  createUser <- jsonData :: ActionM CreateUser
  -- In production, hash the password!
  let userParams = (cuUsername createUser, cuEmail createUser, cuPassword createUser)
  result <- liftIO $ Session.run (Session.statement userParams createUserStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right user -> do
      status status201
      json $ toPublicUser user

-- | PUT /users/:id - Update user
updateUserHandler :: Connection.Connection -> ActionM ()
updateUserHandler conn = do
  uid <- pathParam "id"
  updateUser <- jsonData :: ActionM UpdateUser
  let userParams = (uid, uuUsername updateUser, uuEmail updateUser)
  result <- liftIO $ Session.run (Session.statement userParams updateUserStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right Nothing -> do
      status status404
      json $ object ["error" .= ("User not found" :: String)]
    Right (Just user) -> json $ toPublicUser user

-- | DELETE /users/:id - Delete user
deleteUserHandler :: Connection.Connection -> ActionM ()
deleteUserHandler conn = do
  uid <- pathParam "id"
  result <- liftIO $ Session.run (Session.statement uid deleteUserStatement) conn
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String), "details" .= show err]
    Right rowsAffected -> 
      if rowsAffected > 0
        then do
          status status204
          json $ object ["message" .= ("User deleted" :: String)]
        else do
          status status404
          json $ object ["error" .= ("User not found" :: String)]