{-# LANGUAGE DeriveAnyClass #-}

module DF.Effect.Model.FetchRegistryWithRepo (FetchRegistryWithRepo (..)) where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Git.Repo
import Data.Aeson

data FetchRegistryWithRepo = FetchRegistryWithRepo
  { _basicDependency :: BasicDependency,
    _results :: These DependencyRegistryInfo Repo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON FetchRegistryWithRepo where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions
