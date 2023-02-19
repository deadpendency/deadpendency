{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-deriving-strategies #-}

module Common.Effect.GitHub.WriteChecks.Backend.UpdateCheckRunBackend
  ( githubUpdateCheckRun,
  )
where

import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequest qualified as A
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequestStatus qualified as A
import Common.GitHub.GraphQLCommon
import Common.Model.Error.CommonError
import Common.Model.Git.GitSha
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun qualified as A
import Common.Model.GitHub.Checks.CheckRunConclusion qualified as A
import Common.Model.GitHub.Checks.CheckRunStatus qualified as A
import Common.Model.GitHub.Checks.Output.CheckRunOutput qualified as A
import Common.Model.GitHub.GHNodeId as A
import Data.Morpheus.Client

declareGlobalTypesByName "../resource/minimal-github.graphql" ["UpdateCheckRunInput", "RequestableCheckStatusState", "CheckRunOutput", "CheckConclusionState", "CheckStatusState"]

declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    mutation UpdateCheckRun ($checkRunInput: UpdateCheckRunInput!) {
      updateCheckRun(input: $checkRunInput) {
        checkRun {
          checkRunIdOut: id
          checkRunName: name
          checkRunStatus: status
          checkRunConclusion: conclusion
          repository {
            repositoryIdOut: id
          }
          checkSuite {
            commit {
              oid
            }
          }
        }
      }
    }
  |]

githubUpdateCheckRun :: GHInstallationAuth -> A.CheckRunUpdateRequest -> IO (Either CommonError A.CheckRun)
githubUpdateCheckRun gitAuth request' = do
  eitherRawCheckRun <- mutUpdateCheckRun authToken args
  pure $ asCommonMissingError eitherRawCheckRun >>= mapRunOutput
  where
    args = produceRunInput request'
    authToken = gitAuth ^. #_token

mutUpdateCheckRun :: Text -> UpdateCheckRunArgs -> IO (Either (FetchError UpdateCheckRun) UpdateCheckRun)
mutUpdateCheckRun = fetch . executeGraphQL'

produceRunInput :: A.CheckRunUpdateRequest -> UpdateCheckRunArgs
produceRunInput request' =
  UpdateCheckRunArgs
    { checkRunInput =
        UpdateCheckRunInput
          { repositoryId = repositoryId',
            checkRunId = checkRunId',
            status = status',
            conclusion = conclusion',
            output = output'
          }
    }
  where
    repositoryId' = ID $ request' ^. (#_repoNodeId . #_ntText)
    checkRunId' = ID $ request' ^. (#_checkRunNodeId . #_ntText)
    status' = toGQLCheckRunRequestStatus <$> request' ^. #_checkRunStatus
    conclusion' = toGQLCheckRunConclusion <$> request' ^. #_checkRunConclusion
    output' = produceCheckRunOutput <$> request' ^. #_checkRunOutput

produceCheckRunOutput :: A.CheckRunOutput -> CheckRunOutput
produceCheckRunOutput checkRunOutput =
  CheckRunOutput
    { summary = summary',
      text = Just text',
      title = title'
    }
  where
    title' = checkRunOutput ^. (#_checkRunOutputTitle . #_ntText)
    text' = checkRunOutput ^. (#_checkRunOutputBody . #_ntText)
    summary' = checkRunOutput ^. (#_checkRunOutputSummary . #_ntText)

toGQLCheckRunRequestStatus :: A.CheckRunUpdateRequestStatus -> RequestableCheckStatusState
toGQLCheckRunRequestStatus A.CheckRunUpdateRequestStatusQueued = RequestableCheckStatusStateQUEUED
toGQLCheckRunRequestStatus A.CheckRunUpdateRequestStatusInProgress = RequestableCheckStatusStateIN_PROGRESS
toGQLCheckRunRequestStatus A.CheckRunUpdateRequestStatusCompleted = RequestableCheckStatusStateCOMPLETED

toGQLCheckRunConclusion :: A.CheckRunConclusion -> CheckConclusionState
toGQLCheckRunConclusion A.CheckRunConclusionFailure = CheckConclusionStateFAILURE
toGQLCheckRunConclusion A.CheckRunConclusionSuccess = CheckConclusionStateSUCCESS

mapRunOutput :: UpdateCheckRun -> Either CommonError A.CheckRun
mapRunOutput ccr = first GitHubResponseDecodeError $
  do
    checkRunPayload' <- maybeToRight "CheckRunPayloadMissing" $ updateCheckRun ccr
    checkRun' <- maybeToRight "CheckRunMissing" $ checkRun checkRunPayload'
    let name' = checkRunName checkRun'
        commitSha' = (_gitObjectIDText . oid . commit . checkSuite) checkRun'
        gitSha' = GitSha commitSha'
    let repositoryId' = GHNodeId $ (unpackID . repositoryIdOut . repository) checkRun'
        checkRunId' = GHNodeId $ (unpackID . checkRunIdOut) checkRun'
        status' = toAppStatus $ checkRunStatus checkRun'
        conclusion' = toAppConclusion <$> checkRunConclusion checkRun'
    pure
      A.CheckRun
        { A._nodeId = checkRunId',
          A._headSha = gitSha',
          A._name = name',
          A._repoNodeId = repositoryId',
          A._checkRunStatus = status',
          A._checkRunConclusion = conclusion'
        }

toAppStatus :: CheckStatusState -> A.CheckRunStatus
toAppStatus CheckStatusStateQUEUED = A.CheckRunStatusQueued
toAppStatus CheckStatusStateREQUESTED = A.CheckRunStatusRequested
toAppStatus CheckStatusStateIN_PROGRESS = A.CheckRunStatusInProgress
toAppStatus CheckStatusStateCOMPLETED = A.CheckRunStatusCompleted

toAppConclusion :: CheckConclusionState -> A.CheckRunConclusion
toAppConclusion CheckConclusionStateFAILURE = A.CheckRunConclusionFailure
toAppConclusion CheckConclusionStateSUCCESS = A.CheckRunConclusionSuccess
toAppConclusion _ = error "weird conclusion state"
