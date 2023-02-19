module Common.Model.GitHub.Checks.Output.CheckRunOutputSummary
  ( CheckRunOutputSummary (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype CheckRunOutputSummary = CheckRunOutputSummary
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunOutputSummary where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunOutputSummary where
  parseJSON = genericParseJSON cleanJSONOptions
