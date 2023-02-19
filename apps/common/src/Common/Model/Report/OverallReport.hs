module Common.Model.Report.OverallReport
  ( OverallReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Config.AppVersion
import Common.Model.Report.DependencyReports
import Common.Model.Report.ReportResult
import Data.Aeson

data OverallReport = OverallReport
  { _producedWithVersion :: AppVersion,
    _reportResult :: ReportResult,
    _dependencyReports :: DependencyReports,
    _isTruncated :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OverallReport where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON OverallReport where
  parseJSON = genericParseJSON cleanJSONOptions
