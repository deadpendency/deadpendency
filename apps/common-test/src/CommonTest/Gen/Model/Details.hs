{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Details where

import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import CommonTest.Gen.General
import CommonTest.Gen.Model.Git
import CommonTest.Gen.Model.GitHub
import Hedgehog
import Hedgehog.Gen qualified as Gen

genRun :: Gen Run
genRun = do
  runTrace <- genRunTrace
  maybeGitRef <- Gen.maybe genGitRef
  headSha <- genGitSha
  repoDependencyName <- genGHRepoFullName
  isPrivate <- Gen.bool
  repoOwnerType <- genGHRepoOwnerType
  repoOwnerAccountId <- genPositiveInt
  qualifiedRepo <- genQualifiedRepo
  repoNodeId <- genGHNodeId
  triggeredUser <- genGHUserName
  isDeadpendencyRun <- Gen.bool
  appInstallationId <- genGHAppInstallationId
  maybeInstallationAuth <- Gen.maybe genGHInstallationAuth
  pure
    Run
      { _runTrace = runTrace,
        _gitRef = maybeGitRef,
        _gitHeadSha = headSha,
        _repoDependencyName = repoDependencyName,
        _repoPrivate = isPrivate,
        _repoOwnerType = repoOwnerType,
        _repoOwnerAccountId = repoOwnerAccountId,
        _qualifiedRepo = qualifiedRepo,
        _repoNodeId = repoNodeId,
        _triggeredUser = triggeredUser,
        _isDeadpendencyRun = isDeadpendencyRun,
        _appInstallationId = appInstallationId,
        _ghInstallationAuth = maybeInstallationAuth
      }

genRunTrace :: Gen RunTrace
genRunTrace = RunTrace <$> genAlphaText
