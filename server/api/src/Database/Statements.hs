{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Database.Statements where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Tuple.Curry (uncurryN)
import Data.Profunctor (rmap)
import Hasql.Statement (Statement(..))
import Models.User
import Models.Image
import qualified Hasql.TH as TH
import qualified Data.Vector as V

-- ** User Statements **

-- | Get all users
getAllUsersStatement :: Statement () [User]
getAllUsersStatement = 
  rmap
    (V.toList . fmap (uncurryN User))
    [TH.vectorStatement|
      SELECT id :: int8, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM users ORDER BY id
    |]

-- | Get user by ID
getUserByIdStatement :: Statement Int64 (Maybe User)
getUserByIdStatement = 
  rmap
    (fmap (uncurryN User))
    [TH.maybeStatement|
      SELECT id :: int8, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM users WHERE id = $1 :: int8
    |]

-- | Create a new user
createUserStatement :: Statement (Text, Text, Text) User
createUserStatement = 
  rmap
    (uncurryN User)
    [TH.singletonStatement|
      INSERT INTO users (username, email, password_hash) 
      VALUES ($1 :: text, $2 :: text, $3 :: text)
      RETURNING id :: int8, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Update user
updateUserStatement :: Statement (Int64, Maybe Text, Maybe Text) (Maybe User)
updateUserStatement = 
  rmap
    (fmap (uncurryN User))
    [TH.maybeStatement|
      UPDATE users SET 
        username = COALESCE($2 :: text?, username), 
        email = COALESCE($3 :: text?, email), 
        updated_at = CURRENT_TIMESTAMP 
      WHERE id = $1 :: int8
      RETURNING id :: int8, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Delete user - returns number of rows affected
deleteUserStatement :: Statement Int64 Int64
deleteUserStatement = 
  [TH.rowsAffectedStatement|
    DELETE FROM users WHERE id = $1 :: int8
  |]

-- ** Image Statements **

-- | Get all images
getAllImagesStatement :: Statement () [Image]
getAllImagesStatement = 
  rmap
    (V.toList . fmap (uncurryN Image))
    [TH.vectorStatement|
      SELECT id :: int8, user_id :: int8, file_path :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM images ORDER BY id
    |]

-- | Get image by ID
getImageByIdStatement :: Statement Int64 (Maybe Image)
getImageByIdStatement = 
  rmap
    (fmap (uncurryN Image))
    [TH.maybeStatement|
      SELECT id :: int8, user_id :: int8, file_path :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM images WHERE id = $1 :: int8
    |]

-- | Get images by user ID
getImagesByUserIdStatement :: Statement Int64 [Image]
getImagesByUserIdStatement = 
  rmap
    (V.toList . fmap (uncurryN Image))
    [TH.vectorStatement|
      SELECT id :: int8, user_id :: int8, file_path :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM images WHERE user_id = $1 :: int8
    |]

-- | Create a new image
createImageStatement :: Statement Int64 Image
createImageStatement = 
  rmap
    (uncurryN Image)
    [TH.singletonStatement|
      INSERT INTO images (user_id) VALUES ($1 :: int8)
      RETURNING id :: int8, user_id :: int8, file_path :: text, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Delete image - returns number of rows affected
deleteImageStatement :: Statement Int64 Int64
deleteImageStatement = 
  [TH.rowsAffectedStatement|
    DELETE FROM images WHERE id = $1 :: int8
  |]