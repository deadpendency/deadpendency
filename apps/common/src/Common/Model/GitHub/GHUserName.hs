module Common.Model.GitHub.GHUserName
  ( GHUserName (..),
  )
where

import Common.Aeson.Aeson (cleanJSONOptions)
import Data.Aeson

newtype GHUserName = GHUserName
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq)

instance ToJSON GHUserName where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHUserName where
  parseJSON = genericParseJSON cleanJSONOptions
