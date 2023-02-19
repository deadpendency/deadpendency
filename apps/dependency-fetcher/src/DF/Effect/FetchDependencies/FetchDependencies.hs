{-# LANGUAGE TemplateHaskell #-}

module DF.Effect.FetchDependencies.FetchDependencies
  ( FetchDependencies (..),
    fetchDependencies,
  )
where

import Control.Effect.TH
import DF.Effect.FetchDependencies.Model.FetchDependenciesRequest
import DF.Effect.FetchDependencies.Model.FetchDependenciesResult

data FetchDependencies (m :: Type -> Type) k where
  FetchDependencies :: FetchDependenciesRequest -> FetchDependencies m FetchDependenciesResult

makeSmartConstructors ''FetchDependencies
