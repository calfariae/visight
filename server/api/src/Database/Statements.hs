{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Database.Statements where

import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
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
      SELECT id :: uuid, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM users ORDER BY id
    |]

-- | Get user by ID
getUserByIdStatement :: Statement UUID (Maybe User)
getUserByIdStatement = 
  rmap
    (fmap (uncurryN User))
    [TH.maybeStatement|
      SELECT id :: uuid, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM users WHERE id = $1 :: uuid
    |]

-- | Get user by email
getUserByEmailStatement :: Statement Text (Maybe User)
getUserByEmailStatement = 
  rmap
    (fmap (uncurryN User))
    [TH.maybeStatement|
      SELECT id :: uuid, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
      FROM users WHERE email = $1 :: text
    |]

-- | Create a new user
createUserStatement :: Statement (Text, Text, Text) User
createUserStatement = 
  rmap
    (uncurryN User)
    [TH.singletonStatement|
      INSERT INTO users (username, email, password_hash) 
      VALUES ($1 :: text, $2 :: text, $3 :: text)
      RETURNING id :: uuid, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Update user
updateUserStatement :: Statement (UUID, Maybe Text, Maybe Text) (Maybe User)
updateUserStatement = 
  rmap
    (fmap (uncurryN User))
    [TH.maybeStatement|
      UPDATE users SET 
        username = COALESCE($2 :: text?, username), 
        email = COALESCE($3 :: text?, email), 
        updated_at = CURRENT_TIMESTAMP 
      WHERE id = $1 :: uuid
      RETURNING id :: uuid, username :: text, email :: text, password_hash :: text, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Delete user - returns number of rows affected
deleteUserStatement :: Statement UUID Int64
deleteUserStatement = 
  [TH.rowsAffectedStatement|
    DELETE FROM users WHERE id = $1 :: uuid
  |]

-- ** Image Statements **

-- | Get all images
getAllImagesStatement :: Statement () [Image]
getAllImagesStatement = 
  rmap
    (V.toList . fmap (uncurryN Image))
    [TH.vectorStatement|
      SELECT id :: uuid, user_id :: uuid, file_path :: text, weight :: int8, created_at :: timestamptz, updated_at :: timestamptz
      FROM images ORDER BY id
    |]

-- | Get image by ID
getImageByIdStatement :: Statement UUID (Maybe Image)
getImageByIdStatement = 
  rmap
    (fmap (uncurryN Image))
    [TH.maybeStatement|
      SELECT id :: uuid, user_id :: uuid, file_path :: text, weight :: int8, created_at :: timestamptz, updated_at :: timestamptz
      FROM images WHERE id = $1 :: uuid
    |]

-- | Get images by user ID
getImagesByUserIdStatement :: Statement UUID [Image]
getImagesByUserIdStatement = 
  rmap
    (V.toList . fmap (uncurryN Image))
    [TH.vectorStatement|
      SELECT id :: uuid, user_id :: uuid, file_path :: text, weight :: int8, created_at :: timestamptz, updated_at :: timestamptz
      FROM images WHERE user_id = $1 :: uuid
    |]


-- | Create a new image
createImageStatement :: Statement UUID Image
createImageStatement = 
  rmap
    (uncurryN Image)
    [TH.singletonStatement|
      INSERT INTO images (user_id) VALUES ($1 :: uuid)
      RETURNING id :: uuid, user_id :: uuid, file_path :: text, weight :: int8, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Update image
updateImageStatement :: Statement (UUID, Maybe Int64) (Maybe Image)
updateImageStatement = 
  rmap
    (fmap (uncurryN Image))
    [TH.maybeStatement|
      UPDATE images SET 
        weight = $2 :: int8?,
        updated_at = CURRENT_TIMESTAMP 
      WHERE id = $1 :: uuid
      RETURNING id :: uuid, user_id :: uuid, file_path :: text, weight :: int8, created_at :: timestamptz, updated_at :: timestamptz
    |]

-- | Delete image - returns number of rows affected
deleteImageStatement :: Statement UUID Int64
deleteImageStatement = 
  [TH.rowsAffectedStatement|
    DELETE FROM images WHERE id = $1 :: uuid
  |]