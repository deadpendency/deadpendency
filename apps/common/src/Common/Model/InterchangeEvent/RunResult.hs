module Common.Model.InterchangeEvent.RunResult
  ( RunResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.ProcessingError
import Common.Model.Report.OverallReport
import Data.Aeson

data RunResult
  = RunSuccess OverallReport
  | RunFailure ProcessingError
  deriving stock (Eq, Show, Generic)

instance ToJSON RunResult where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RunResult where
  parseJSON = genericParseJSON cleanJSONOptions
