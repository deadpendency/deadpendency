module Common.Model.Dependency.Repo.DependencyRepoCommit
  ( DependencyRepoCommit (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data DependencyRepoCommit = DependencyRepoCommit
  { _commitDate :: UTCTime,
    _commitAuthorEmail :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyRepoCommit where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyRepoCommit where
  parseJSON = genericParseJSON cleanJSONOptions
