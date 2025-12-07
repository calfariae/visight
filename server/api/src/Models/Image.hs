{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Image where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Image model
data Image = Image
  { imageId :: UUID
  , imageUserId :: UUID
  , imagePath :: Text
  , imageWeight :: Int64
  , imageCreatedAt :: UTCTime
  , imageUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON Image
instance FromJSON Image

-- | DTO for creating a new image
data CreateImage = CreateImage
  { ciUserId :: UUID
  } deriving (Show, Generic)

instance FromJSON CreateImage

-- | DTO for updating an image
data UpdateImage = UpdateImage
  { uiImageWeight :: Maybe Int64
  } deriving (Show, Generic)

instance FromJSON UpdateImage