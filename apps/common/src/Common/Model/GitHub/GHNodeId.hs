module Common.Model.GitHub.GHNodeId
  ( GHNodeId (..),
  )
where

import Common.Aeson.Aeson (cleanJSONOptions)
import Data.Aeson

newtype GHNodeId = GHNodeId
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq)

instance ToJSON GHNodeId where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHNodeId where
  parseJSON = genericParseJSON cleanJSONOptions
