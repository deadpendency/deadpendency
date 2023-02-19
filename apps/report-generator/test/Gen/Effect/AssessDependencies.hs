{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.Effect.AssessDependencies where

import CommonTest.Gen.General
import CommonTest.Gen.Model.Assessment
import Hedgehog
import Hedgehog.Range qualified as Range
import RG.Effect.AssessDependencies.Model.AssessDependenciesResult

genAssessDependenciesResult :: Gen AssessDependenciesResult
genAssessDependenciesResult =
  AssessDependenciesResult
    <$> genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
