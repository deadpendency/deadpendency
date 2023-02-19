module RG.Effect.GenerateReport.Model.GenerateReportRequest
  ( GenerateReportRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessment
import Common.Model.Dependency.Errored.ErroredRepoDependencies
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data GenerateReportRequest = GenerateReportRequest
  { _dependencyAssessments :: NV.NonEmptyVector DependencyAssessment,
    _erroredRepoDependencies :: ErroredRepoDependencies,
    _ignoredRepoDependencies :: IgnoredRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GenerateReportRequest where
  toJSON = genericToJSON cleanJSONOptions
