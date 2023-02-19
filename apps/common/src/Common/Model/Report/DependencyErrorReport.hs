module Common.Model.Report.DependencyErrorReport
  ( DependencyErrorReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Report.PackageLink
import Data.Aeson

data DependencyErrorReport = DependencyErrorReport
  { _dependencyIdentifier :: DependencyIdentifier,
    _dependencyPackageLink :: Maybe PackageLink
  }
  deriving stock (Eq, Show, Generic)

instance Ord DependencyErrorReport where
  compare (DependencyErrorReport di _) (DependencyErrorReport di' _) =
    compare di di'

instance ToJSON DependencyErrorReport where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyErrorReport where
  parseJSON = genericParseJSON cleanJSONOptions
