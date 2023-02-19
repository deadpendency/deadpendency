module Common.Effect.GitHub.FetchRepoFiles.Backend.Model.InternalRepoFileRequest
  ( InternalRepoFileRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Data.Aeson

data InternalRepoFileRequest = InternalRepoFileRequest
  { _qualifiedRepo :: QualifiedRepo,
    _commitSha :: GitSha,
    _filePath :: GitPath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON InternalRepoFileRequest where
  toJSON = genericToJSON cleanJSONOptions
