{-# LANGUAGE TemplateHaskell #-}

module RG.Effect.AssessDependencies.AssessDependencies
  ( AssessDependencies (..),
    assessDependencies,
  )
where

import Control.Effect.TH
import RG.Effect.AssessDependencies.Model.AssessDependenciesRequest
import RG.Effect.AssessDependencies.Model.AssessDependenciesResult

data AssessDependencies (m :: Type -> Type) k where
  AssessDependencies :: AssessDependenciesRequest -> AssessDependencies m AssessDependenciesResult

makeSmartConstructors ''AssessDependencies
