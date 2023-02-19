{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.GitHub.TriggerCheckSuite.TriggerCheckSuite
  ( githubTriggerCheckSuite,
  )
where

import Common.GitHub.GraphQLCommon
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHNodeId as A
import Data.Morpheus.Client

declareGlobalTypesByName "../resource/minimal-github.graphql" ["RerequestCheckSuiteInput"]

declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    mutation RerequestCheckSuite ($rerequestCheckSuiteInput: RerequestCheckSuiteInput!) {
      rerequestCheckSuite(input: $rerequestCheckSuiteInput) {
        checkSuite {
          checkRuns(first: 1) {
            edges {
              node {
                checkRunId: id
              }
            }
          }
        }
      }
    }
  |]

githubTriggerCheckSuite :: GHInstallationAuth -> GHNodeId -> GHNodeId -> IO (Either CommonError GHNodeId)
githubTriggerCheckSuite gitAuth repoId checkSuiteId = do
  eitherRawCheckRun <- mutTriggerCheckSuite authToken args
  pure $ asCommonMissingError eitherRawCheckRun >>= mapRunOutput
  where
    args = produceRunInput repoId checkSuiteId
    authToken = gitAuth ^. #_token

mutTriggerCheckSuite :: Text -> RerequestCheckSuiteArgs -> IO (Either (FetchError RerequestCheckSuite) RerequestCheckSuite)
mutTriggerCheckSuite = fetch . executeGraphQL

produceRunInput :: GHNodeId -> GHNodeId -> RerequestCheckSuiteArgs
produceRunInput (GHNodeId repoIdText) (GHNodeId checkSuiteIdText) =
  RerequestCheckSuiteArgs
    { rerequestCheckSuiteInput =
        RerequestCheckSuiteInput
          { repositoryId = ID repoIdText,
            checkSuiteId = ID checkSuiteIdText
          }
    }

mapRunOutput :: RerequestCheckSuite -> Either CommonError GHNodeId
mapRunOutput rcs = first GitHubResponseDecodeError $
  do
    rerequestCheckSuitePayload' <- maybeToRight "RerequestCheckSuitePayloadMissing" $ rerequestCheckSuite rcs
    checkSuite' <- maybeToRight "CheckSuiteMissing" $ checkSuite rerequestCheckSuitePayload'
    checkRuns' <- maybeToRight "CheckRunsMissing" $ checkRuns checkSuite'
    listMaybeEdges' <- maybeToRight "CheckRunEdgesMissing" $ edges checkRuns'
    listMaybeCheckRuns' <- node <<$>> maybesToEither "Some checkruns are null" listMaybeEdges'
    case listMaybeCheckRuns' of
      ((Just checkRun') : _) -> Right $ GHNodeId $ (unpackID . checkRunId) checkRun'
      _ -> Left "No check runs found"
