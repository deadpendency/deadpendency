{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.GitHub.GetCheckRunOutput.GetCheckRunOutput
  ( githubGetCheckRunOutput,
  )
where

import Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputRequest
import Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputResult
import Common.GitHub.GraphQLCommon
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRunConclusion qualified as A
import Common.Model.GitHub.Checks.CheckRunStatus qualified as A
import Common.Model.GitHub.Checks.Output.CheckRunOutput qualified as A
import Common.Model.GitHub.Checks.Output.CheckRunOutputBody qualified as A
import Common.Model.GitHub.Checks.Output.CheckRunOutputSummary qualified as A
import Common.Model.GitHub.Checks.Output.CheckRunOutputTitle qualified as A
import Common.Model.GitHub.GHNodeId
import Data.Morpheus.Client

declareGlobalTypesByName "../resource/minimal-github.graphql" ["CheckConclusionState", "CheckStatusState"]

declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    query GetCheckRunOutput ($repoOwner: String!, $repoName: String!, $commit: GitObjectID!, $appId: Int!) {
      repository(name: $repoName, owner: $repoOwner) {
        object(oid: $commit) {
          __typename
          ... on Commit {
            checkSuites(first: 1, filterBy: { appId: $appId }) {
              checkSuiteEdges: edges {
                checkSuiteNode: node {
                  checkRuns (first:1) {
                    checkRunEdges: edges {
                      checkRunNode: node {
                        checkRunId: id
                        status
                        conclusion
                        title
                        summary
                        text
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  |]

githubGetCheckRunOutput :: GHInstallationAuth -> GetCheckRunOutputRequest -> IO (Either CommonError GetCheckRunOutputResult)
githubGetCheckRunOutput gitAuth request' = do
  eitherRawCheckRun <- mutGetCheckRunOutput authToken args
  pure $ asCommonMissingError eitherRawCheckRun >>= mapRunOutput
  where
    args = produceRunInput request'
    authToken = gitAuth ^. #_token

mutGetCheckRunOutput :: Text -> GetCheckRunOutputArgs -> IO (Either (FetchError GetCheckRunOutput) GetCheckRunOutput)
mutGetCheckRunOutput = fetch . executeGraphQL

produceRunInput :: GetCheckRunOutputRequest -> GetCheckRunOutputArgs
produceRunInput request' =
  GetCheckRunOutputArgs
    { repoOwner = repoOwner',
      repoName = repoName',
      commit = commitShaObject',
      appId = appId'
    }
  where
    repoOwner' = request' ^. (#_qualifiedRepo . #_repoOwner . #_ntText)
    repoName' = request' ^. (#_qualifiedRepo . #_repoName . #_ntText)
    commitShaObject' = GitObjectID $ request' ^. (#_commitSha . #_ntText)
    appId' = request' ^. #_appId

mapRunOutput :: GetCheckRunOutput -> Either CommonError GetCheckRunOutputResult
mapRunOutput rcs = first GitHubResponseDecodeError $
  do
    repository' <- maybeToRight "Repository missing from fetch repo stats" $ repository rcs
    repositoryObject' <- maybeToRight "Repository Commit Missing" $ object repository'
    repositoryObjectCommit' <- getCommitFromObject repositoryObject'
    checkSuitesConnection' <- maybeToRight "CheckSuiteMissing" $ checkSuites repositoryObjectCommit'
    listMaybeCheckSuiteNodes' <- maybeToRight "Check Suite connection missing" $ checkSuiteEdges checkSuitesConnection'
    listMaybeCheckSuites' <- checkSuiteNode <<$>> maybesToEither "Some nodes are null for get check suite edges" listMaybeCheckSuiteNodes'
    listCheckSuites' <- maybesToEither "Some check suites are null" listMaybeCheckSuites'
    firstCheckSuite' <- maybeToRight "No check suites returned" $ viaNonEmpty head listCheckSuites'
    checkRunsConnection' <- maybeToRight "CheckRunMissing" $ checkRuns firstCheckSuite'
    listMaybeCheckRunNodes' <- maybeToRight "Check Run connection missing" $ checkRunEdges checkRunsConnection'
    listMaybeCheckRuns' <- checkRunNode <<$>> maybesToEither "Some nodes are null for get check run edges" listMaybeCheckRunNodes'
    listCheckRuns' <- maybesToEither "Some check runs are null" listMaybeCheckRuns'
    firstCheckRun' <- maybeToRight "No check runs returned" $ viaNonEmpty head listCheckRuns'
    let id' = GHNodeId . unpackID $ checkRunId firstCheckRun'
        status' = toAppStatus $ status firstCheckRun'
        conclusion' = toAppConclusion <$> conclusion firstCheckRun'
        output' = do
          title' <- title firstCheckRun'
          summary' <- summary firstCheckRun'
          text' <- text firstCheckRun'
          pure
            A.CheckRunOutput
              { A._checkRunOutputTitle = A.CheckRunOutputTitle title',
                A._checkRunOutputBody = A.CheckRunOutputBody text',
                A._checkRunOutputSummary = A.CheckRunOutputSummary summary'
              }

    pure
      GetCheckRunOutputResult
        { _checkRunId = id',
          _output = output',
          _status = status',
          _conclusion = conclusion'
        }

getCommitFromObject :: GetCheckRunOutputRepositoryObject -> Either Text GetCheckRunOutputRepositoryObjectCommit
getCommitFromObject (GetCheckRunOutputRepositoryObjectVariantCommit commit) = Right commit
getCommitFromObject (GetCheckRunOutputRepositoryObjectVariantGitObject _) = Left "Unexpected git object"

toAppStatus :: CheckStatusState -> A.CheckRunStatus
toAppStatus CheckStatusStateQUEUED = A.CheckRunStatusQueued
toAppStatus CheckStatusStateREQUESTED = A.CheckRunStatusRequested
toAppStatus CheckStatusStateIN_PROGRESS = A.CheckRunStatusInProgress
toAppStatus CheckStatusStateCOMPLETED = A.CheckRunStatusCompleted

toAppConclusion :: CheckConclusionState -> A.CheckRunConclusion
toAppConclusion CheckConclusionStateFAILURE = A.CheckRunConclusionFailure
toAppConclusion CheckConclusionStateSUCCESS = A.CheckRunConclusionSuccess
toAppConclusion _ = error "weird conclusion state"
