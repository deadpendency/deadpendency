{-# LANGUAGE DeriveAnyClass #-}

module DF.Effect.FetchDependencies.Model.FetchDependencyRegistryException
  ( FetchDependencyRegistryException (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.ConsideredAppFailure
import Data.Aeson

newtype FetchDependencyRegistryException
  = FDRERegistryFetchExceptional Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ConsideredAppFailure FetchDependencyRegistryException where
  consideredAppFailure =
    \case
      FDRERegistryFetchExceptional _ -> True

instance ToJSON FetchDependencyRegistryException where
  toJSON = genericToJSON cleanJSONOptions
