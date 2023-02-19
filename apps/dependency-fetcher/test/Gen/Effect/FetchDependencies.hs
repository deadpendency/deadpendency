{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.Effect.FetchDependencies where

import CommonTest.Gen.Model.Dependency
import DF.Effect.FetchDependencies.Model.FetchDependenciesResult
import Hedgehog

genFetchDependenciesResult :: Gen FetchDependenciesResult
genFetchDependenciesResult =
  FetchDependenciesResult
    <$> genEnrichedRepoDependencies
    <*> genErroredRepoDependencies
