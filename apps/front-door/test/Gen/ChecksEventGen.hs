{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.ChecksEventGen where

import CommonTest.Gen.General
import Data.Vector qualified as V
import Gen.CommonEventGen
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genCheckSuiteEvent :: Gen CheckSuiteEvent
genCheckSuiteEvent = do
  action <- genCheckSuiteEventAction
  checkSuite <- genHookCheckSuite
  repository <- genHookRepository
  organization <- Just <$> genHookOrganization
  sender <- genHookUser
  installation <- Just <$> genHookChecksInstallation
  pure
    CheckSuiteEvent
      { evCheckSuiteAction = action,
        evCheckSuiteCheckSuite = checkSuite,
        evCheckSuiteRepository = repository,
        evCheckSuiteOrganization = organization,
        evCheckSuiteSender = sender,
        evCheckSuiteInstallation = installation
      }

genCheckSuiteEventAction :: Gen CheckSuiteEventAction
genCheckSuiteEventAction = do
  otherText <- genAlphaText
  Gen.element
    [ CheckSuiteEventActionCompleted,
      CheckSuiteEventActionRequested,
      CheckSuiteEventActionRerequested,
      CheckSuiteEventActionOther otherText
    ]

genHookCheckSuite :: Gen HookCheckSuite
genHookCheckSuite = do
  id' <- Gen.int Range.constantBounded
  nodeId <- genAlphaText
  maybeBranch <- Gen.maybe genAlphaText
  headSha <- genAlphaText
  status <- genHookCheckSuiteStatus
  maybeConclusion <- Gen.maybe genHookCheckSuiteConclusion
  url' <- genUrl
  maybeBeforeSha <- Gen.maybe genAlphaText
  maybeAfterSha <- Gen.maybe genAlphaText
  let pullRequests = V.empty
  createdAtTime <- genUTCTime
  updatedAtTime <- genUTCTime
  maybeRunsCount <- Gen.maybe $ Gen.int (Range.constant 1 10)
  maybeCheckRunUrl <- Gen.maybe genUrl
  maybeHeadCommit <- Gen.maybe genHookCheckSuiteCommit
  pure
    HookCheckSuite
      { whCheckSuiteId = id',
        whCheckSuiteNodeId = nodeId,
        whCheckSuiteHeadBranch = maybeBranch,
        whCheckSuiteHeadSha = headSha,
        whCheckSuiteStatus = status,
        whCheckSuiteConclusion = maybeConclusion,
        whCheckSuiteUrl = url',
        whCheckSuiteBeforeSha = maybeBeforeSha,
        whCheckSuiteAfterSha = maybeAfterSha,
        whCheckSuitePullRequests = pullRequests,
        whCheckSuiteCreatedAt = createdAtTime,
        whCheckSuiteUpdatedAt = updatedAtTime,
        whCheckSuiteLatestCheckRunsCount = maybeRunsCount,
        whCheckSuiteCheckRunsUrl = maybeCheckRunUrl,
        whCheckSuiteHeadCommit = maybeHeadCommit
      }

genHookCheckSuiteStatus :: Gen HookCheckSuiteStatus
genHookCheckSuiteStatus = do
  otherText <- genAlphaText
  Gen.element
    [ HookCheckSuiteStatusRequested,
      HookCheckSuiteStatusQueued,
      HookCheckSuiteStatusInProgress,
      HookCheckSuiteStatusCompleted,
      HookCheckSuiteStatusOther otherText
    ]

genHookCheckSuiteConclusion :: Gen HookCheckSuiteConclusion
genHookCheckSuiteConclusion = do
  otherText <- genAlphaText
  Gen.element
    [ HookCheckSuiteConclusionSuccess,
      HookCheckSuiteConclusionFailure,
      HookCheckSuiteConclusionNeutral,
      HookCheckSuiteConclusionCancelled,
      HookCheckSuiteConclusionTimedOut,
      HookCheckSuiteConclusionActionRequired,
      HookCheckSuiteConclusionStale,
      HookCheckSuiteConclusionOther otherText
    ]

genHookCheckSuiteCommit :: Gen HookCheckSuiteCommit
genHookCheckSuiteCommit = do
  commitSha <- genAlphaText
  author <- genHookSimpleUser
  committer <- genHookSimpleUser
  pure
    HookCheckSuiteCommit
      { whCheckSuiteCommitSha = commitSha,
        whCheckSuiteCommitAuthor = author,
        whCheckSuiteCommitCommitter = committer
      }

genHookChecksInstallation :: Gen HookChecksInstallation
genHookChecksInstallation = do
  id' <- genPositiveInt
  nodeId <- genAlphaText
  pure
    HookChecksInstallation
      { whChecksInstallationId = id',
        whChecksInstallationNodeId = nodeId
      }

genCheckRunEvent :: Gen CheckRunEvent
genCheckRunEvent = do
  action <- genCheckRunEventAction
  checkRun <- genHookCheckRun
  checkRunRequestedAction <- Gen.maybe genHookCheckRunRequestedAction
  repository <- genHookRepository
  organization <- Just <$> genHookOrganization
  sender <- genHookUser
  installation <- Just <$> genHookChecksInstallation
  pure
    CheckRunEvent
      { evCheckRunAction = action,
        evCheckRunCheckRun = checkRun,
        evCheckRunRequestedAction = checkRunRequestedAction,
        evCheckRunRepository = repository,
        evCheckRunOrganization = organization,
        evCheckRunSender = sender,
        evCheckRunInstallation = installation
      }

genCheckRunEventAction :: Gen CheckRunEventAction
genCheckRunEventAction = do
  otherText <- genAlphaText
  Gen.element
    [ CheckRunEventActionCreated,
      CheckRunEventActionCompleted,
      CheckRunEventActionRerequested,
      CheckRunEventActionRequestedAction,
      CheckRunEventActionOther otherText
    ]

genHookCheckRunRequestedAction :: Gen HookCheckRunRequestedAction
genHookCheckRunRequestedAction = HookCheckRunRequestedAction <$> genAlphaText

genHookCheckRun :: Gen HookCheckRun
genHookCheckRun = do
  id' <- Gen.int Range.constantBounded
  nodeId <- genAlphaText
  headSha <- genAlphaText
  externalId <- genAlphaText
  url' <- genUrl
  htmlUrl <- genUrl
  detailsUrl <- genUrl
  status <- genHookCheckRunStatus
  maybeConclusion <- Gen.maybe genHookCheckRunConclusion
  startedAtTime <- genUTCTime
  maybeCompletedAtTime <- Gen.maybe genUTCTime
  checkRunOutput <- genHookCheckRunOutput
  checkRunName <- genAlphaText
  hookCheckSuite <- genHookCheckSuite
  let pullRequests = V.empty
  pure
    HookCheckRun
      { whCheckRunId = id',
        whCheckRunNodeId = nodeId,
        whCheckRunHeadSha = headSha,
        whCheckRunExternalId = externalId,
        whCheckRunUrl = url',
        whCheckRunHtmlUrl = htmlUrl,
        whCheckRunDetailsUrl = detailsUrl,
        whCheckRunStatus = status,
        whCheckRunConclusion = maybeConclusion,
        whCheckRunStartedAt = startedAtTime,
        whCheckRunCompletedAt = maybeCompletedAtTime,
        whCheckRunOutput = checkRunOutput,
        whCheckRunName = checkRunName,
        whCheckRunCheckSuite = hookCheckSuite,
        whCheckRunPullRequests = pullRequests
      }

genHookCheckRunStatus :: Gen HookCheckRunStatus
genHookCheckRunStatus = do
  otherText <- genAlphaText
  Gen.element
    [ HookCheckRunStatusInProgress,
      HookCheckRunStatusCompleted,
      HookCheckRunStatusOther otherText
    ]

genHookCheckRunConclusion :: Gen HookCheckRunConclusion
genHookCheckRunConclusion = do
  otherText <- genAlphaText
  Gen.element
    [ HookCheckRunConclusionSuccess,
      HookCheckRunConclusionFailure,
      HookCheckRunConclusionNeutral,
      HookCheckRunConclusionCancelled,
      HookCheckRunConclusionTimedOut,
      HookCheckRunConclusionActionRequired,
      HookCheckRunConclusionStale,
      HookCheckRunConclusionOther otherText
    ]

genHookCheckRunOutput :: Gen HookCheckRunOutput
genHookCheckRunOutput = do
  title <- Gen.maybe genAlphaText
  summary <- Gen.maybe genAlphaText
  text <- Gen.maybe genAlphaText
  annotationsCount <- Gen.int Range.constantBounded
  annotationsUrl <- genUrl
  pure
    HookCheckRunOutput
      { whCheckRunOutputTitle = title,
        whCheckRunOutputSummary = summary,
        whCheckRunOutputText = text,
        whCheckRunOutputAnnotationsCount = annotationsCount,
        whCheckRunOutputAnnotationsUrl = annotationsUrl
      }
