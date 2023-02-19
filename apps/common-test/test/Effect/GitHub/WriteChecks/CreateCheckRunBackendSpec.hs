module Effect.GitHub.WriteChecks.CreateCheckRunBackendSpec (spec) where

import Common.Effect.GitHub.WriteChecks.Backend.CreateCheckRunBackend
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import Common.Model.Config.AppEnv
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitSha
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.GitHub.Checks.CheckRunStatus
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import CommonTest.GitHub.TestGitHubAuth
import Control.Lens ((^?!))
import Data.Vector qualified as V
import Test.Hspec

spec :: Spec
spec =
  -- requires secrets which no longer exist in GCP
  xcontext "CreateCheckRunBackendSpec" $
    beforeAll (reloadOrGenerateAuth Test) $
      it "creates a happy day check run" $ \installAuth -> do
        let createCheckRunRequest =
              CheckRunCreateRequest
                { _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test",
                  _repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyMzQ4NTc4NTQ=",
                  _appInstallationId = GHAppInstallationId 51614
                }
        eitherCheckRunInternalResult <- liftIO $ githubCreateCheckRun installAuth createCheckRunRequest
        -- this changes every time.. not sure of a nice way to ignore this from the expectation check, so we set it
        let crNodeId' = eitherCheckRunInternalResult ^?! (_Right . #_checkRun . #_nodeId)
        let expectedCheckRun =
              CheckRun
                { _nodeId = crNodeId',
                  _headSha = GitSha "33755d79dcd2c1a119f2e6c113ef2f339186a16a",
                  _name = "Integration Test",
                  _repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyMzQ4NTc4NTQ=",
                  _checkRunStatus = CheckRunStatusInProgress,
                  _checkRunConclusion = Nothing
                }
            expectedInternalResult =
              CheckRunCreateResult
                { _checkRun = expectedCheckRun,
                  _repoProgrammingLanguages = V.fromList [JavaScript, UnsupportedLanguage "Shell"]
                }

        eitherCheckRunInternalResult `shouldBe` Right expectedInternalResult
