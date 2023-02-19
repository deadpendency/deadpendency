module Common.Model.Git.GitSha
  ( GitSha (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GitSha = GitSha
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq)

instance ToJSON GitSha where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GitSha where
  parseJSON = genericParseJSON cleanJSONOptions
