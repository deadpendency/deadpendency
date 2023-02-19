{-# LANGUAGE TemplateHaskell #-}

module DF.Effect.FetchRepoStats.FetchRepoStats
  ( FetchRepoStats (..),
    fetchRepoStats,
  )
where

import Control.Effect.TH
import DF.Effect.FetchRepoStats.Model.DependencyFetchResult
import DF.Effect.Model.FetchRegistryWithRepo
import Data.Vector.NonEmpty qualified as NV

data FetchRepoStats (m :: Type -> Type) k where
  FetchRepoStats :: NV.NonEmptyVector FetchRegistryWithRepo -> FetchRepoStats m (NV.NonEmptyVector DependencyFetchResult)

makeSmartConstructors ''FetchRepoStats
