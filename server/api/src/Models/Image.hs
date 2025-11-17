{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Image where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Image model
data Image = Image
  { imageId :: Int64
  , imageUserId :: Int64
  , imageCreatedAt :: UTCTime
  , imageUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON Image
instance FromJSON Image

-- | DTO for creating a new image
data CreateImage = CreateImage
  { ciUserId :: Int64
  } deriving (Show, Generic)

instance FromJSON CreateImage