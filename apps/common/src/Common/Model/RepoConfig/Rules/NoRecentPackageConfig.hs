module Common.Model.RepoConfig.Rules.NoRecentPackageConfig
  ( NoRecentPackageConfig (..),
    defaultNoRecentPackageConfig,
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data NoRecentPackageConfig = NoRecentPackageConfig
  { _warnAtMonths :: Maybe Int,
    _failAtMonths :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

defaultNoRecentPackageConfig :: NoRecentPackageConfig
defaultNoRecentPackageConfig =
  NoRecentPackageConfig
    { _warnAtMonths = Just 18,
      _failAtMonths = Just 24
    }

instance ToJSON NoRecentPackageConfig where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON NoRecentPackageConfig where
  parseJSON = genericParseJSON cleanJSONOptions
