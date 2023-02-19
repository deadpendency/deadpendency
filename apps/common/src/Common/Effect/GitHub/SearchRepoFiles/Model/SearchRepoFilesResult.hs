module Common.Effect.GitHub.SearchRepoFiles.Model.SearchRepoFilesResult
  ( SearchRepoFilesResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Data.Aeson
import Data.Vector qualified as V

newtype SearchRepoFilesResult = SearchRepoFilesResult
  { _repoFilePaths :: V.Vector GitPath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchRepoFilesResult where
  toJSON = genericToJSON cleanJSONOptions
