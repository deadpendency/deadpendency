module Handler.RunFinalizerHandlerSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadMockC
import Common.Effect.Trace.Carrier.TraceEmitRetC
import CommonTest.Gen.Effect.TraceEmit
import CommonTest.Gen.Gogol.PubSub
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.InterchangeEvent
import Gen.Effect.FinishRun
import Hedgehog.Gen (sample)
import RF.Effect.FinishRun.Carrier.FinishRunRetC
import RF.Handler.RunFinalizerHandler
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "When processing events" $
    it "a happy day test produces correct results" $
      hedgehog $ do
        reportGenerated <- forAll (genInterchangeEvent genRunResult)
        receivedMessage <- sample genPubSubReceivedMessage
        finishRunResult <- sample genFinishRunResult
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let runHandler =
              runTraceEmitRet runTrace startSpanResult
                >>> runAppEventEmitRet
                >>> mockInterchangeEventLoad reportGenerated
                >>> runFinishRunRet finishRunResult
                >>> runIdentity

        let handlerResult = runFinalizerHandler "asdf/asdf" receivedMessage
            (writtenFinishRunRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, _)))) = runHandler handlerResult

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

        annotate "writes a finish run request"
        genericLength @Integer writtenFinishRunRequests === 1
