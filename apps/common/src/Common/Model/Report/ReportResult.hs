module Common.Model.Report.ReportResult
  ( ReportResult (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data ReportResult
  = ReportResultPass
  | ReportResultWarning
  | ReportResultFail
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON ReportResult where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON ReportResult where
  parseJSON = genericParseJSON cleanJSONOptions
