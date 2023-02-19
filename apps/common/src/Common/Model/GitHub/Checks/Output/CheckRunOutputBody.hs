module Common.Model.GitHub.Checks.Output.CheckRunOutputBody
  ( CheckRunOutputBody (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype CheckRunOutputBody = CheckRunOutputBody
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunOutputBody where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunOutputBody where
  parseJSON = genericParseJSON cleanJSONOptions
