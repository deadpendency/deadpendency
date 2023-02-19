module Common.Model.Dependency.File.DependenciesFileLoad
  ( DependenciesFileLoad (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.File.DependenciesFileLoadDetails
import Common.Model.Dependency.File.DependenciesFileType
import Data.Aeson

data DependenciesFileLoad = DependenciesFileLoad
  { _type :: DependenciesFileType,
    _loadDetails :: DependenciesFileLoadDetails
  }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON DependenciesFileLoad where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependenciesFileLoad where
  parseJSON = genericParseJSON cleanJSONOptions
