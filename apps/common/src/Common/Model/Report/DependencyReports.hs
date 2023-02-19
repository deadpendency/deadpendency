{-# LANGUAGE DataKinds #-}

module Common.Model.Report.DependencyReports
  ( DependencyReports (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Report.DependencyErrorReports
import Common.Model.Report.DependencyFailureReport
import Common.Model.Report.DependencyIgnoreReport
import Common.Model.Report.DependencyLanguageReport
import Common.Model.Report.DependencyPassReport
import Common.Model.Report.DependencyWarningReport
import Data.Aeson
import Data.Vector qualified as V

data DependencyReports
  = DRSingleLanguageReports (V.Vector DependencyPassReport) (V.Vector DependencyWarningReport) (V.Vector DependencyFailureReport) (V.Vector DependencyIgnoreReport) (V.Vector DependencyErrorReports)
  | DRMultiLanguageReports (V.Vector (DependencyLanguageReport DependencyPassReport)) (V.Vector (DependencyLanguageReport DependencyWarningReport)) (V.Vector (DependencyLanguageReport DependencyFailureReport)) (V.Vector (DependencyLanguageReport DependencyIgnoreReport)) (V.Vector (DependencyLanguageReport DependencyErrorReports))
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyReports where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyReports where
  parseJSON = genericParseJSON cleanJSONOptions
