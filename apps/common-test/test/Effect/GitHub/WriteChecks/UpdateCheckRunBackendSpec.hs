module Effect.GitHub.WriteChecks.UpdateCheckRunBackendSpec (spec) where

import Common.Effect.GitHub.WriteChecks.Backend.CreateCheckRunBackend
import Common.Effect.GitHub.WriteChecks.Backend.UpdateCheckRunBackend
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequestStatus
import Common.Model.Config.AppEnv
import Common.Model.Git.GitSha
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.CheckRunStatus
import Common.Model.GitHub.Checks.Output.CheckRunOutput
import Common.Model.GitHub.Checks.Output.CheckRunOutputBody
import Common.Model.GitHub.Checks.Output.CheckRunOutputSummary
import Common.Model.GitHub.Checks.Output.CheckRunOutputTitle
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import CommonTest.GitHub.TestGitHubAuth
import Control.Lens ((^?!))
import Test.Hspec

spec :: Spec
spec =
  -- requires secrets which no longer exist in GCP
  xcontext "UpdateCheckRunBackendSpec" $
    beforeAll (reloadOrGenerateAuth Test) $ do
      it "updates a happy day check run" $ \installAuth -> do
        let repositoryId = GHNodeId "MDEwOlJlcG9zaXRvcnkyMzQ4NTc4NTQ="
            createCheckRunRequest =
              CheckRunCreateRequest
                { _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test",
                  _repoNodeId = repositoryId,
                  _appInstallationId = GHAppInstallationId 51614
                }
        eitherCreatedCheckRunResult <- githubCreateCheckRun installAuth createCheckRunRequest
        -- this changes every time.. not sure of a nice way to ignore this from the expectation check, so we set it
        let eitherCheckRun = eitherCreatedCheckRunResult <&> _checkRun
            crNodeId' = eitherCheckRun ^?! (_Right . #_nodeId)

        let expectedCreatedCheckRun =
              CheckRun
                { _nodeId = crNodeId',
                  _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test",
                  _repoNodeId = repositoryId,
                  _checkRunStatus = CheckRunStatusInProgress,
                  _checkRunConclusion = Nothing
                }

        -- confirm we are in the intial created state
        eitherCheckRun `shouldBe` Right expectedCreatedCheckRun

        let updateCheckRunOutput =
              CheckRunOutput
                { _checkRunOutputTitle = CheckRunOutputTitle "test-title",
                  _checkRunOutputBody = CheckRunOutputBody "test-body",
                  _checkRunOutputSummary = CheckRunOutputSummary "test-summary"
                }
            updateCheckRunRequest =
              CheckRunUpdateRequest
                { _repoNodeId = repositoryId,
                  _checkRunNodeId = crNodeId',
                  _checkRunStatus = Just CheckRunUpdateRequestStatusQueued,
                  _checkRunConclusion = Nothing,
                  _checkRunOutput = Just updateCheckRunOutput
                }

        eitherUpdatedCheckRun <- githubUpdateCheckRun installAuth updateCheckRunRequest

        let expectedUpdatedCheckRun =
              CheckRun
                { _nodeId = crNodeId',
                  _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test",
                  _repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyMzQ4NTc4NTQ=",
                  _checkRunStatus = CheckRunStatusQueued,
                  _checkRunConclusion = Nothing
                }

        eitherUpdatedCheckRun `shouldBe` Right expectedUpdatedCheckRun

      it "completes a happy day check run" $ \installAuth -> do
        let repositoryId = GHNodeId "MDEwOlJlcG9zaXRvcnkyMzQ4NTc4NTQ="
            createCheckRunRequest =
              CheckRunCreateRequest
                { _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test Completion",
                  _repoNodeId = repositoryId,
                  _appInstallationId = GHAppInstallationId 51614
                }
        eitherCreatedCheckRunResult <- githubCreateCheckRun installAuth createCheckRunRequest
        -- this changes every time.. not sure of a nice way to ignore this from the expectation check, so we set it
        let eitherCheckRun = eitherCreatedCheckRunResult <&> _checkRun
            crNodeId' = eitherCheckRun ^?! (_Right . #_nodeId)

        let expectedCreatedCheckRun =
              CheckRun
                { _nodeId = crNodeId',
                  _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test Completion",
                  _repoNodeId = repositoryId,
                  _checkRunStatus = CheckRunStatusInProgress,
                  _checkRunConclusion = Nothing
                }

        -- confirm we are in the intial created state
        eitherCheckRun `shouldBe` Right expectedCreatedCheckRun

        let updateCheckRunOutput =
              CheckRunOutput
                { _checkRunOutputTitle = CheckRunOutputTitle "test-title",
                  _checkRunOutputBody = CheckRunOutputBody "test-body",
                  _checkRunOutputSummary = CheckRunOutputSummary "test-summary"
                }
            updateCheckRunRequest =
              CheckRunUpdateRequest
                { _repoNodeId = repositoryId,
                  _checkRunNodeId = crNodeId',
                  _checkRunStatus = Just CheckRunUpdateRequestStatusCompleted,
                  _checkRunConclusion = Just CheckRunConclusionSuccess,
                  _checkRunOutput = Just updateCheckRunOutput
                }

        eitherUpdatedCheckRun <- githubUpdateCheckRun installAuth updateCheckRunRequest

        let expectedUpdatedCheckRun =
              CheckRun
                { _nodeId = crNodeId',
                  _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test Completion",
                  _repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyMzQ4NTc4NTQ=",
                  _checkRunStatus = CheckRunStatusCompleted,
                  _checkRunConclusion = Just CheckRunConclusionSuccess
                }

        eitherUpdatedCheckRun `shouldBe` Right expectedUpdatedCheckRun
