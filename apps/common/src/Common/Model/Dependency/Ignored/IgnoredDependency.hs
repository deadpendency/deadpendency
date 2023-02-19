module Common.Model.Dependency.Ignored.IgnoredDependency
  ( IgnoredDependency (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson

data IgnoredDependency = IgnoredDependency
  { _programmingLanguage :: ProgrammingLanguage,
    _dependencyIdentifier :: DependencyIdentifier,
    _dependencyType :: Maybe DependencyType
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON IgnoredDependency where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON IgnoredDependency where
  parseJSON = genericParseJSON cleanJSONOptions
