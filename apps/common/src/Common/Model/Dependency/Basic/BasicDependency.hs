{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.Basic.BasicDependency
  ( BasicDependency (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson

data BasicDependency = BasicDependency
  { _programmingLanguage :: ProgrammingLanguage,
    _dependencyIdentifier :: DependencyIdentifier,
    _dependencyType :: Maybe DependencyType
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance ToJSON BasicDependency where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON BasicDependency where
  parseJSON = genericParseJSON cleanJSONOptions
