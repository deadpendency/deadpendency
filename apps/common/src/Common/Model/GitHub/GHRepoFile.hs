module Common.Model.GitHub.GHRepoFile
  ( GHRepoFile (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitPath
import Data.Aeson

data GHRepoFile = GHRepoFile
  { _filePath :: GitPath,
    _fileContents :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GHRepoFile where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHRepoFile where
  parseJSON = genericParseJSON cleanJSONOptions
