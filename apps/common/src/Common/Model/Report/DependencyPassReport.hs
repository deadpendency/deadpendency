module Common.Model.Report.DependencyPassReport
  ( DependencyPassReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Git.Repo
import Common.Model.Report.PackageLink
import Data.Aeson

data DependencyPassReport = DependencyPassReport
  { _dependencyIdentifier :: DependencyIdentifier,
    _dependencyRepo :: Maybe Repo,
    _dependencyPackageLink :: Maybe PackageLink
  }
  deriving stock (Eq, Show, Generic)

instance Ord DependencyPassReport where
  compare (DependencyPassReport di _ _) (DependencyPassReport di' _ _) =
    compare di di'

instance ToJSON DependencyPassReport where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyPassReport where
  parseJSON = genericParseJSON cleanJSONOptions
