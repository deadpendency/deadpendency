module Common.Model.Details.RunTrace
  ( RunTrace (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype RunTrace = RunTrace
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RunTrace where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RunTrace where
  parseJSON = genericParseJSON cleanJSONOptions
