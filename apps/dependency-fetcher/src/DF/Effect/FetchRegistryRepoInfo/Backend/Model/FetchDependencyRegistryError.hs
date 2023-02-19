{-# LANGUAGE DeriveAnyClass #-}

module DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
  ( FetchDependencyRegistryError (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data FetchDependencyRegistryError
  = FDRFailureToParseResult Text
  | FDRRegistryFetchExceptional Text
  | FDRRegistryDataInconsistent Text
  | FDRDependencyNameInvalid Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON FetchDependencyRegistryError where
  toJSON = genericToJSON cleanJSONOptions
