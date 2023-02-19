{-# LANGUAGE TemplateHaskell #-}

module DF.Effect.FetchRegistryRepoInfo.FetchRegistryRepoInfo
  ( FetchRegistryRepoInfo (..),
    fetchRegistryRepoInfo,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Control.Effect.TH
import DF.Effect.FetchRegistryRepoInfo.Model.FetchRegistryRepoInfoResult
import Data.Vector.NonEmpty qualified as NV

data FetchRegistryRepoInfo (m :: Type -> Type) k where
  FetchRegistryRepoInfo :: NV.NonEmptyVector BasicDependency -> FetchRegistryRepoInfo m (NV.NonEmptyVector FetchRegistryRepoInfoResult)

makeSmartConstructors ''FetchRegistryRepoInfo
