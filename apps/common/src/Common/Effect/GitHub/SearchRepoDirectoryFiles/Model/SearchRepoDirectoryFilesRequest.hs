module Common.Effect.GitHub.SearchRepoDirectoryFiles.Model.SearchRepoDirectoryFilesRequest
  ( SearchRepoDirectoryFilesRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitFileMatch
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Data.Aeson

data SearchRepoDirectoryFilesRequest = SearchRepoDirectoryFilesRequest
  { _filesMatch :: GitFileMatch,
    _directoryPath :: GitPath,
    _qualifiedRepo :: QualifiedRepo,
    _commitSha :: GitSha
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchRepoDirectoryFilesRequest where
  toJSON = genericToJSON cleanJSONOptions
