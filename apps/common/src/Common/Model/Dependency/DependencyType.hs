{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.DependencyType
  ( DependencyType (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data DependencyType
  = CoreDependency
  | DevDependency
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance ToJSON DependencyType where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyType where
  parseJSON = genericParseJSON cleanJSONOptions
