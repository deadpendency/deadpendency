module Common.Model.GitHub.Checks.Output.CheckRunOutput
  ( CheckRunOutput (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.GitHub.Checks.Output.CheckRunOutputBody
import Common.Model.GitHub.Checks.Output.CheckRunOutputSummary
import Common.Model.GitHub.Checks.Output.CheckRunOutputTitle
import Data.Aeson

data CheckRunOutput = CheckRunOutput
  { _checkRunOutputTitle :: CheckRunOutputTitle,
    _checkRunOutputBody :: CheckRunOutputBody,
    _checkRunOutputSummary :: CheckRunOutputSummary
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunOutput where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunOutput where
  parseJSON = genericParseJSON cleanJSONOptions
