module Common.Model.GitHub.GHAppId
  ( GHAppId (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GHAppId = GHAppId
  { _ntInt :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GHAppId where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHAppId where
  parseJSON = genericParseJSON cleanJSONOptions
