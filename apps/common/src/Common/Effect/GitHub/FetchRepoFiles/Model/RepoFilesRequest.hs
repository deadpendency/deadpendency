module Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesRequest
  ( RepoFilesRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data RepoFilesRequest = RepoFilesRequest
  { _qualifiedRepo :: QualifiedRepo,
    _commitSha :: GitSha,
    _filePaths :: NV.NonEmptyVector GitPath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RepoFilesRequest where
  toJSON = genericToJSON cleanJSONOptions
