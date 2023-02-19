module Handler.RunPreparerHandlerSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadMockC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultRetC
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.Trace.Carrier.TraceEmitRetC
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.InterchangeEvent.RunPrepared
import Common.Model.RepoConfig.RepoConfig
import CommonTest.Gen.Effect.TraceEmit
import CommonTest.Gen.Gogol.PubSub
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.InterchangeEvent
import CommonTest.Gen.Model.RepoConfig
import Control.Carrier.Error.Either (runError)
import Control.Carrier.State.Strict (runState)
import Hedgehog.Gen (sample)
import RP.Effect.ReadConfig.Carrier.ReadConfigRetC
import RP.Effect.ReadConfig.Model.ReadConfigError
import RP.Effect.ReadConfig.Model.ReadConfigRequest
import RP.Effect.ReadConfig.Model.ReadConfigResult
import RP.Effect.VerifyPlan.Carrier.VerifyPlanRetC
import RP.Handler.RunPreparerHandler (runPreparerHandler)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "When processing events" $
    it "a happy day test produces correct results" $
      hedgehog $ do
        run <- forAll genRun
        riCheckRunCreated <- sample $ genInterchangeEvent genCheckRunCreated
        receivedMessage <- sample genPubSubReceivedMessage
        repoConfig <- sample genRepoConfig
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let massagedRun =
              run
                & (#_gitHeadSha . #_ntText) .~ "head-sha"
                & (#_qualifiedRepo . #_repoOwner . #_ntText) .~ "repo-owner"
                & (#_qualifiedRepo . #_repoName . #_ntText) .~ "repo-name"
                & (#_qualifiedRepo . #_repoHost) .~ GitHub
            massagedRiCheckRunCreated =
              riCheckRunCreated
                & #_run .~ massagedRun
            readConfigResult = ReadConfigResult repoConfig

        let runHandler =
              runVerifyPlanRet
                >>> runError @ReadConfigError
                >>> runState @(Maybe RepoConfig) Nothing
                >>> runTraceEmitRet runTrace startSpanResult
                >>> runAppEventEmitRet
                >>> mockInterchangeEventLoad massagedRiCheckRunCreated
                >>> runReadConfigRet readConfigResult
                >>> runPublishComponentResultRet @RunPrepared
                >>> runIdentity

        let handlerResult = runPreparerHandler "asdf/asdf" receivedMessage
            (simpleResults, (writtenReadConfigRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, (putRepoConfig, readConfigErrors)))))) = runHandler handlerResult

        let expectedRunPrepared =
              SuccessComponentResult $
                RunPrepared
                  { _repoProgrammingLanguages = riCheckRunCreated ^. (#_result . #_repoProgrammingLanguages)
                  }

        annotate "did not throw any config errors"
        isRight readConfigErrors === True

        annotate "produces the correct run prepared event"
        simpleResults === [expectedRunPrepared]

        annotate "and put the repo config into state"
        putRepoConfig === Just repoConfig

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

        let expectedReadConfigRequest =
              ReadConfigRequest
                { _gitSha = GitSha "head-sha",
                  _gHQualifiedRepo = QualifiedRepo GitHub (RepoOwner "repo-owner") (RepoName "repo-name")
                }

        annotate "make the correct read config request"
        writtenReadConfigRequests === [expectedReadConfigRequest]
