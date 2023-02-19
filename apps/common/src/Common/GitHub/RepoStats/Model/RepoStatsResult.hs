module Common.GitHub.RepoStats.Model.RepoStatsResult
  ( RepoStatsResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Repo.DependencyRepoStats
import Data.Aeson

newtype RepoStatsResult = RepoStatsResult
  { _dependencyRepoStats :: Maybe DependencyRepoStats
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RepoStatsResult where
  toJSON = genericToJSON cleanJSONOptions
