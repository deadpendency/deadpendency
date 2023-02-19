module Common.Model.Config.AppVersion (AppVersion (..)) where

import Common.Aeson.Aeson
import Data.Aeson

newtype AppVersion = AppVersion
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AppVersion where
  toJSON = genericToJSON cleanJSONOptions

instance FromJSON AppVersion where
  parseJSON = genericParseJSON cleanJSONOptions
