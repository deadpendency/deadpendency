module Common.Model.Git.GitFileMatch
  ( GitFileMatch (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GitFileMatch = GitFileMatch
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq, Ord)

instance ToJSON GitFileMatch where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GitFileMatch where
  parseJSON = genericParseJSON cleanJSONOptions
