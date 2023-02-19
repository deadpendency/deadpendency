module Handler.CheckRunCreatorHandlerSpec (spec) where

import CRC.Handler.CheckRunCreatorHandler (checkRunCreatorHandler)
import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.GitHub.WriteChecks.Carrier.WriteChecksRetC
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
import Common.Effect.InterchangeEventDecode.Carrier.InterchangeEventDecodeMockC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultRetC
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.Trace.Carrier.TraceEmitRetC
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Git.GitSha
import Common.Model.Git.RepoHost
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import Common.Model.InterchangeEvent.CheckRunCreated
import Common.Model.InterchangeEvent.RunCreated
import Common.Model.RepoConfig.RepoConfig
import CommonTest.Gen.Effect.TraceEmit
import CommonTest.Gen.General
import CommonTest.Gen.Gogol.PubSub
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.GitHub.Checks.API
import Control.Carrier.State.Strict (runState)
import Hedgehog.Gen (sample)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "When processing events" $
    it "a happy day test produces correct results" $
      hedgehog $ do
        run <- forAll genRun
        runCreatedResult <- forAll genCheckRunCreateResult
        receivedMessage <- sample genPubSubReceivedMessage
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult
        googleTrace <- sample genAlphaText
        let massagedRun =
              run
                & (#_gitHeadSha . #_ntText) .~ "head-sha"
                & (#_repoNodeId . #_ntText) .~ "repo-node-id"
                & (#_qualifiedRepo . #_repoOwner . #_ntText) .~ "repo-owner"
                & (#_qualifiedRepo . #_repoName . #_ntText) .~ "repo-name"
                & (#_qualifiedRepo . #_repoHost) .~ GitHub
                & (#_appInstallationId . #_ntInt) .~ 1234
                & #_runTrace .~ runTrace
            runCreated = RunCreated massagedRun

        let runHandler =
              runState @(Maybe RepoConfig) Nothing
                >>> runState @(Maybe CheckRun) Nothing
                >>> runState @(Maybe Run) Nothing
                >>> runState @(Maybe RunTrace) Nothing
                >>> runTraceEmitRet runTrace startSpanResult
                >>> runAppEventEmitRet
                >>> mockInterchangeEventDecode runCreated
                >>> runWriteChecksRet runCreatedResult
                >>> runPublishComponentResultRet @CheckRunCreated
                >>> runIdentity

        let handlerResult = checkRunCreatorHandler googleTrace receivedMessage
            (simpleResults, (writtenChecksRequest, (appEvents, (concludeSpanRequests, (startSpanRequests, (putRunTrace, (putRun, _ {- putCheckRun -}))))))) = runHandler handlerResult

        let expectedRunCheckRunCreated =
              SuccessComponentResult $
                CheckRunCreated
                  { _repoProgrammingLanguages = runCreatedResult ^. #_repoProgrammingLanguages
                  }

        annotate "produces the correct check run created event"
        simpleResults === [expectedRunCheckRunCreated]

        annotate "and put the run into state"
        putRun === Just massagedRun

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        let expectedRunTrace = Just runTrace

        annotate "the correct run trace state was put"
        putRunTrace === expectedRunTrace

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

        let expectedCheckRunCreateRequest =
              CheckRunCreateRequest
                { _headSha = GitSha "head-sha",
                  _name = "Deadpendency Check",
                  _repoNodeId = GHNodeId "repo-node-id",
                  _appInstallationId = GHAppInstallationId 1234
                }

        annotate "writes the correct write checks request"
        writtenChecksRequest === [expectedCheckRunCreateRequest]
