module Common.Model.RepoConfig.Rules.NoRecentCommitConfig
  ( NoRecentCommitConfig (..),
    defaultNoRecentCommitConfig,
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data NoRecentCommitConfig = NoRecentCommitConfig
  { _warnAtMonths :: Maybe Int,
    _failAtMonths :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

defaultNoRecentCommitConfig :: NoRecentCommitConfig
defaultNoRecentCommitConfig =
  NoRecentCommitConfig
    { _warnAtMonths = Just 12,
      _failAtMonths = Just 18
    }

instance ToJSON NoRecentCommitConfig where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON NoRecentCommitConfig where
  parseJSON = genericParseJSON cleanJSONOptions
