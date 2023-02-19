module Common.Model.Report.PackageLink
  ( PackageLink (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.Registry
import Data.Aeson

data PackageLink = PackageLink
  { _registry :: Registry,
    _dependencyName :: DependencyName
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PackageLink where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON PackageLink where
  parseJSON = genericParseJSON cleanJSONOptions
