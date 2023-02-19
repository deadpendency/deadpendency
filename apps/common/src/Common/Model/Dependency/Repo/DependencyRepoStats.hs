module Common.Model.Dependency.Repo.DependencyRepoStats
  ( DependencyRepoStats (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Data.Aeson
import Data.Vector qualified as V

data DependencyRepoStats = DependencyRepoStats
  { _twoYearlyCommitHistory :: V.Vector DependencyRepoCommit,
    _isArchived :: Bool,
    _isFork :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyRepoStats where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyRepoStats where
  parseJSON = genericParseJSON cleanJSONOptions
