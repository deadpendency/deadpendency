{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.Registry.RegistryAlivenessStatus
  ( RegistryAlivenessStatus (..),
    RegistryAlivenessStatusType (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyName
import Data.Aeson
import Data.Vector qualified as V

data RegistryAlivenessStatus
  = RASAlive
  | RASDeprecated RegistryAlivenessStatusType (Maybe Text) (V.Vector DependencyName)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data RegistryAlivenessStatusType
  = RASTDeprecated
  | RASTAbandoned
  | RASTRelocated
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (NFData)

instance ToJSON RegistryAlivenessStatus where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RegistryAlivenessStatus where
  parseJSON = genericParseJSON cleanJSONOptions

instance ToJSON RegistryAlivenessStatusType where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RegistryAlivenessStatusType where
  parseJSON = genericParseJSON cleanJSONOptions
