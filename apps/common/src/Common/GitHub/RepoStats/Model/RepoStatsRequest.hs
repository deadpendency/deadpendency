module Common.GitHub.RepoStats.Model.RepoStatsRequest
  ( RepoStatsRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.QualifiedRepo
import Data.Aeson

newtype RepoStatsRequest = RepoStatsRequest
  { _qualifiedRepo :: QualifiedRepo
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RepoStatsRequest where
  toJSON = genericToJSON cleanJSONOptions
