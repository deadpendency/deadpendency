{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.InterchangeEvent where

import Common.Model.InterchangeEvent.CheckRunCreated
import Common.Model.InterchangeEvent.DependenciesDetermined
import Common.Model.InterchangeEvent.DependenciesFetched
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunResult
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.Ecosystem
import CommonTest.Gen.Model.Error
import CommonTest.Gen.Model.GitHub
import CommonTest.Gen.Model.GitHub.Checks.CheckRun
import CommonTest.Gen.Model.RepoConfig
import CommonTest.Gen.Model.Report
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genInterchangeEvent :: Gen a -> Gen (InterchangeEvent a)
genInterchangeEvent genInner = do
  ghInstallationAuth <- genGHInstallationAuth
  checkRun <- genCheckRun
  run <- genRun
  inner <- genInner
  repoConfig <- genRepoConfig
  pure
    InterchangeEvent
      { _ghInstallationAuth = ghInstallationAuth,
        _checkRun = checkRun,
        _run = run,
        _repoConfig = repoConfig,
        _knownFailureOccurred = False,
        _result = inner
      }

genCheckRunCreated :: Gen CheckRunCreated
genCheckRunCreated = do
  programmingLanguages <- genVector (Range.constant 0 10) genProgrammingLanguage
  pure
    CheckRunCreated
      { _repoProgrammingLanguages = programmingLanguages
      }

genDependenciesDetermined :: Gen DependenciesDetermined
genDependenciesDetermined = do
  basicRepoDependencies <- genBasicRepoDependencies
  ignoredRepoDependencies <- genIgnoredRepoDependencies
  pure
    DependenciesDetermined
      { _basicRepoDependencies = basicRepoDependencies,
        _ignoredRepoDependencies = ignoredRepoDependencies
      }

genDependenciesFetched :: Gen DependenciesFetched
genDependenciesFetched = do
  enrichedRepoDependencies <- genEnrichedRepoDependencies
  ignoredRepoDependencies <- genIgnoredRepoDependencies
  erroredRepoDependencies <- genErroredRepoDependencies
  pure
    DependenciesFetched
      { _enrichedRepoDependencies = enrichedRepoDependencies,
        _erroredRepoDependencies = erroredRepoDependencies,
        _ignoredRepoDependencies = ignoredRepoDependencies
      }

genRunResult :: Gen RunResult
genRunResult =
  Gen.choice
    [ RunSuccess <$> genOverallReport,
      RunFailure <$> genProcessingError
    ]
