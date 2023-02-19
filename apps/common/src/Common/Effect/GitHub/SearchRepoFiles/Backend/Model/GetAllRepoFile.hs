module Common.Effect.GitHub.SearchRepoFiles.Backend.Model.GetAllRepoFile
  ( GetAllRepoFile (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Data.Aeson

data GetAllRepoFile = GetAllRepoFile
  { _fileName :: Text,
    _fullPath :: GitPath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GetAllRepoFile where
  toJSON = genericToJSON cleanJSONOptions
