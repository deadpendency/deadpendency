{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.GitHub.Checks.API where

import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import CommonTest.Gen.General
import CommonTest.Gen.Model.Ecosystem
import CommonTest.Gen.Model.GitHub.Checks.CheckRun
import Hedgehog
import Hedgehog.Range qualified as Range

genCheckRunCreateResult :: Gen CheckRunCreateResult
genCheckRunCreateResult = do
  checkRun <- genCheckRun
  programmingLanguages <- genVector (Range.constant 0 2) genProgrammingLanguage
  pure
    CheckRunCreateResult
      { _checkRun = checkRun,
        _repoProgrammingLanguages = programmingLanguages
      }
