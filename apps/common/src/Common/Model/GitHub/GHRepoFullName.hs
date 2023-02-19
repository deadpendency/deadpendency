module Common.Model.GitHub.GHRepoFullName
  ( GHRepoFullName (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype GHRepoFullName = GHRepoFullName
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq)

instance ToJSON GHRepoFullName where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHRepoFullName where
  parseJSON = genericParseJSON cleanJSONOptions
