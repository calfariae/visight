{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | User model representing a database user
data User = User
  { userId :: UUID
  , username :: Text
  , email :: Text
  , passwordHash :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

-- | DTO for creating a new user (no ID or timestamps)
data CreateUser = CreateUser
  { cuUsername :: Text
  , cuEmail :: Text
  , cuPassword :: Text
  } deriving (Show, Generic)

instance FromJSON CreateUser

-- | DTO for updating a user
data UpdateUser = UpdateUser
  { uuUsername :: Maybe Text
  , uuEmail :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON UpdateUser

-- | Public user representation (without password hash)
data PublicUser = PublicUser
  { puId :: UUID
  , puUsername :: Text
  , puEmail :: Text
  , puCreatedAt :: UTCTime
  , puUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON PublicUser

-- | Convert User to PublicUser
toPublicUser :: User -> PublicUser
toPublicUser u = PublicUser
  { puId = userId u
  , puUsername = username u
  , puEmail = email u
  , puCreatedAt = createdAt u
  , puUpdatedAt = updatedAt u
  }