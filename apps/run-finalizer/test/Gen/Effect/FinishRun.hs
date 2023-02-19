{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.Effect.FinishRun where

import CommonTest.Gen.Model.GitHub.Checks.CheckRun
import Hedgehog
import RF.Effect.FinishRun.Model.FinishRunResult

genFinishRunResult :: Gen FinishRunResult
genFinishRunResult = FinishRunResult <$> genCheckRun
