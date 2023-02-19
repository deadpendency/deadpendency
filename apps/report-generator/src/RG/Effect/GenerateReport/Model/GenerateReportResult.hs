module RG.Effect.GenerateReport.Model.GenerateReportResult
  ( GenerateReportResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Report.OverallReport
import Data.Aeson

newtype GenerateReportResult = GenerateReportResult
  { _dependencyReport :: OverallReport
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GenerateReportResult where
  toJSON = genericToJSON cleanJSONOptions
