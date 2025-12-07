{-# LANGUAGE OverloadedStrings #-}

module Handlers.Users where

import Models.User
import Crypto.BCrypt
import Database.Statements
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
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

-- | POST /login - Authenticate user
loginHandler :: Connection.Connection -> ActionM ()
loginHandler conn = do
  loginUser <- jsonData :: ActionM LoginUser
  
  -- Fetch user by email
  result <- liftIO $ Session.run 
    (Session.statement (luEmail loginUser) getUserByEmailStatement) conn
  
  case result of
    Left err -> do
      status status500
      json $ object ["error" .= ("Database error" :: String)]
    
    Right Nothing -> do
      status status401
      json $ object ["error" .= ("Invalid credentials" :: String)]
    
    Right (Just user) -> do
      -- Verify password
      let isValid = validatePassword 
            (encodeUtf8 $ passwordHash user)
            (encodeUtf8 $ luPassword loginUser)
      
      if isValid
        then json $ object 
          [ "success" .= True
          , "user" .= toPublicUser user
          ]
        else do
          status status401
          json $ object ["error" .= ("Invalid credentials" :: String)]

-- | POST /users - Create a new user
createUserHandler :: Connection.Connection -> ActionM ()
createUserHandler conn = do
  createUser <- jsonData :: ActionM CreateUser
  
  -- Hash the password with bcrypt
  maybeHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy 
                          (encodeUtf8 $ cuPassword createUser)
  
  case maybeHash of
    Nothing -> do
      status status500
      json $ object ["error" .= ("Password hashing failed" :: String)]
    Just hash -> do
      let passwordHash = decodeUtf8 hash
      let userParams = (cuUsername createUser, cuEmail createUser, passwordHash)
      
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