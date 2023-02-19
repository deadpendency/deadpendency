module Common.Model.GitHub.GHAppInstallationId
  ( GHAppInstallationId (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GHAppInstallationId = GHAppInstallationId
  { _ntInt :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GHAppInstallationId where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHAppInstallationId where
  parseJSON = genericParseJSON cleanJSONOptions
