{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.Model.DetermineDependencies.API where

import CommonTest.Gen.Model.Dependency
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesResult
import Hedgehog

genDetermineDependenciesResult :: Gen DetermineDependenciesResult
genDetermineDependenciesResult =
  DetermineDependenciesResult
    <$> genBasicRepoDependencies
    <*> genIgnoredRepoDependencies
