module Common.Effect.GitHub.SearchRepoDirectoryFiles.Backend.Model.GetDirectoryRepoFile
  ( GetDirectoryRepoFile (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Data.Aeson

data GetDirectoryRepoFile = GetDirectoryRepoFile
  { _fileName :: Text,
    _fullPath :: GitPath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GetDirectoryRepoFile where
  toJSON = genericToJSON cleanJSONOptions
