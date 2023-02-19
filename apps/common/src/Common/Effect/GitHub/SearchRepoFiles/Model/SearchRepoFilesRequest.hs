module Common.Effect.GitHub.SearchRepoFiles.Model.SearchRepoFilesRequest
  ( SearchRepoFilesRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitFileMatch
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Data.Aeson

data SearchRepoFilesRequest = SearchRepoFilesRequest
  { _filesMatch :: GitFileMatch,
    _qualifiedRepo :: QualifiedRepo,
    _commitSha :: GitSha
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchRepoFilesRequest where
  toJSON = genericToJSON cleanJSONOptions
