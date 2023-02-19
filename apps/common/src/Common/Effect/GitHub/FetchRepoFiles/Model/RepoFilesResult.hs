module Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesResult
  ( RepoFilesResult (..),
    RepoFileResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Common.Model.GitHub.GHRepoFile
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

newtype RepoFilesResult = RepoFilesResult
  { _repoFiles :: NV.NonEmptyVector RepoFileResult
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RepoFilesResult where
  toJSON = genericToJSON cleanJSONOptions

data RepoFileResult = RepoFileResult
  { _filePath :: GitPath,
    _repoFile :: Maybe GHRepoFile
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RepoFileResult where
  toJSON = genericToJSON cleanJSONOptions
