{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.Errored.ErroredDependency
  ( ErroredDependency (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson

data ErroredDependency = ErroredDependency
  { _dependencyIdentifier :: DependencyIdentifier,
    _dependencyType :: Maybe DependencyType,
    _programmingLanguage :: ProgrammingLanguage,
    _registryInfo :: Maybe DependencyRegistryInfo,
    _erroredReason :: ErroredReason
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ErroredDependency where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON ErroredDependency where
  parseJSON = genericParseJSON cleanJSONOptions
