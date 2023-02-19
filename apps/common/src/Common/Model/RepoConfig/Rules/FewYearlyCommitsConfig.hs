module Common.Model.RepoConfig.Rules.FewYearlyCommitsConfig
  ( FewYearlyCommitsConfig (..),
    defaultFewYearlyCommitsConfig,
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data FewYearlyCommitsConfig = FewYearlyCommitsConfig
  { _warnAtCount :: Maybe Int,
    _failAtCount :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

defaultFewYearlyCommitsConfig :: FewYearlyCommitsConfig
defaultFewYearlyCommitsConfig =
  FewYearlyCommitsConfig
    { _warnAtCount = Just 2,
      _failAtCount = Nothing
    }

instance ToJSON FewYearlyCommitsConfig where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON FewYearlyCommitsConfig where
  parseJSON = genericParseJSON cleanJSONOptions
