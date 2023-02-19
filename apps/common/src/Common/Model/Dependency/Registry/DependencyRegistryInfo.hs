{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.Registry.DependencyRegistryInfo
  ( DependencyRegistryInfo (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.Repo
import Data.Aeson

data DependencyRegistryInfo = DependencyRegistryInfo
  { _registry :: Registry,
    _sourceRepo :: Maybe Repo,
    _alivenessStatus :: RegistryAlivenessStatus,
    _lastReleaseDateTime :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON DependencyRegistryInfo where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyRegistryInfo where
  parseJSON = genericParseJSON cleanJSONOptions
