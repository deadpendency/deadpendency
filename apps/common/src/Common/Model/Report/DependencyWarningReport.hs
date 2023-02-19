module Common.Model.Report.DependencyWarningReport
  ( DependencyWarningReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Git.Repo
import Common.Model.Report.PackageLink
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data DependencyWarningReport = DependencyWarningReport
  { _dependencyIdentifier :: DependencyIdentifier,
    _dependencyRepo :: Maybe Repo,
    _dependencyPackageLink :: Maybe PackageLink,
    _dependencyAssessmentWarnings :: NV.NonEmptyVector DependencyAssessmentWarning
  }
  deriving stock (Eq, Show, Generic)

instance Ord DependencyWarningReport where
  compare (DependencyWarningReport di _ _ _) (DependencyWarningReport di' _ _ _) =
    compare di di'

instance ToJSON DependencyWarningReport where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyWarningReport where
  parseJSON = genericParseJSON cleanJSONOptions
