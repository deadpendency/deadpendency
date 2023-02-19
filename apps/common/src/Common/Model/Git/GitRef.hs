module Common.Model.Git.GitRef
  ( GitRef (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GitRef = GitRef
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq)

instance ToJSON GitRef where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GitRef where
  parseJSON = genericParseJSON cleanJSONOptions
