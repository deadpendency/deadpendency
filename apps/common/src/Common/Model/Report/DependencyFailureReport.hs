module Common.Model.Report.DependencyFailureReport
  ( DependencyFailureReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Git.Repo
import Common.Model.Report.PackageLink
import Data.Aeson
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

data DependencyFailureReport = DependencyFailureReport
  { _dependencyIdentifier :: DependencyIdentifier,
    _dependencyRepo :: Maybe Repo,
    _dependencyPackageLink :: Maybe PackageLink,
    _dependencyAssessmentWarnings :: V.Vector DependencyAssessmentWarning,
    _dependencyAssessmentFailures :: NV.NonEmptyVector DependencyAssessmentFailure
  }
  deriving stock (Eq, Show, Generic)

instance Ord DependencyFailureReport where
  compare (DependencyFailureReport di _ _ _ _) (DependencyFailureReport di' _ _ _ _) =
    compare di di'

instance ToJSON DependencyFailureReport where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyFailureReport where
  parseJSON = genericParseJSON cleanJSONOptions
