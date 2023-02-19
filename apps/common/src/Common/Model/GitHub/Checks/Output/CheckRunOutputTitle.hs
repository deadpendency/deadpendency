module Common.Model.GitHub.Checks.Output.CheckRunOutputTitle
  ( CheckRunOutputTitle (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype CheckRunOutputTitle = CheckRunOutputTitle
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunOutputTitle where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunOutputTitle where
  parseJSON = genericParseJSON cleanJSONOptions
