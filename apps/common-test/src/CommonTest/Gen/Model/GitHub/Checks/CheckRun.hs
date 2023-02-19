{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.GitHub.Checks.CheckRun where

import Common.Model.GitHub.Checks.CheckRun
import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.CheckRunStatus
import CommonTest.Gen.General
import CommonTest.Gen.Model.Git
import CommonTest.Gen.Model.GitHub
import Hedgehog
import Hedgehog.Gen qualified as Gen

genCheckRun :: Gen CheckRun
genCheckRun = do
  nodeId <- genGHNodeId
  headSha <- genGitSha
  name <- genAlphaText
  repositoryNodeId <- genGHNodeId
  checkRunStatus <- genCheckRunStatus
  maybeCheckRunConclusion <- Gen.maybe genCheckRunConclusion
  pure
    CheckRun
      { _nodeId = nodeId,
        _headSha = headSha,
        _name = name,
        _repoNodeId = repositoryNodeId,
        _checkRunStatus = checkRunStatus,
        _checkRunConclusion = maybeCheckRunConclusion
      }

genCheckRunStatus :: Gen CheckRunStatus
genCheckRunStatus = Gen.enumBounded

genCheckRunConclusion :: Gen CheckRunConclusion
genCheckRunConclusion = Gen.enumBounded
