module Common.Model.Git.GitPath
  ( GitPath (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GitPath = GitPath
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq, Ord)

instance ToJSON GitPath where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GitPath where
  parseJSON = genericParseJSON cleanJSONOptions
