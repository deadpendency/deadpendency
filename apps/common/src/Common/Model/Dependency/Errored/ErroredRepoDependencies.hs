{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.Errored.ErroredRepoDependencies
  ( ErroredRepoDependencies (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Errored.ErroredDependency
import Data.Aeson
import Data.Vector qualified as V

newtype ErroredRepoDependencies = ErroredRepoDependencies
  { _dependencies :: V.Vector ErroredDependency
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ErroredRepoDependencies where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON ErroredRepoDependencies where
  parseJSON = genericParseJSON cleanJSONOptions
