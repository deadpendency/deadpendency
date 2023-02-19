{-# LANGUAGE TemplateHaskell #-}

module DD.Effect.DetermineDependencies.DetermineDependencies
  ( DetermineDependencies (..),
    determineDependencies,
  )
where

import Control.Effect.TH
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesRequest
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesResult

data DetermineDependencies (m :: Type -> Type) k where
  DetermineDependencies :: DetermineDependenciesRequest -> DetermineDependencies m DetermineDependenciesResult

makeSmartConstructors ''DetermineDependencies
