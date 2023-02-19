{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.Errored.ErroredReason
  ( ErroredReason (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data ErroredReason
  = UnexpectedFailureToParseRegistryEntry Text
  | UnexpectedFailureRegistryDataInconsistent Text
  | UnexpectedDependencyNameInvalid Text
  | NoRegistryOrRepoData
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ErroredReason where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON ErroredReason where
  parseJSON = genericParseJSON cleanJSONOptions
