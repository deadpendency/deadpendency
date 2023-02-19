module Common.Effect.GitHub.SearchRepoDirectoryFiles.Model.SearchRepoDirectoryFilesResult
  ( SearchRepoDirectoryFilesResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Data.Aeson
import Data.Vector qualified as V

newtype SearchRepoDirectoryFilesResult = SearchRepoDirectoryFilesResult
  { _repoFilePaths :: V.Vector GitPath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchRepoDirectoryFilesResult where
  toJSON = genericToJSON cleanJSONOptions
