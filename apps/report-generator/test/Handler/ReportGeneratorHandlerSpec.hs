module Handler.ReportGeneratorHandlerSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadMockC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultRetC
import Common.Effect.Trace.Carrier.TraceEmitRetC
import Common.Model.InterchangeEvent.RunResult
import CommonTest.Gen.Effect.TraceEmit
import CommonTest.Gen.Gogol.PubSub
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.InterchangeEvent
import Gen.Effect.AssessDependencies
import Gen.Effect.GenerateReport
import Hedgehog.Gen (sample)
import RG.Effect.AssessDependencies.Carrier.AssessDependenciesRetC
import RG.Effect.GenerateReport.Carrier.GenerateReportRetC
import RG.Handler.ReportGeneratorHandler
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "When processing events" $
    it "a happy day test produces correct results" $
      hedgehog $ do
        dependenciesFetched <- forAll (genInterchangeEvent genDependenciesFetched)
        assessDependenciesResult <- forAll genAssessDependenciesResult
        generateReportResult <- forAll genGenerateReportResult
        receivedMessage <- sample genPubSubReceivedMessage
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let runHandler =
              runTraceEmitRet runTrace startSpanResult
                >>> runAppEventEmitRet
                >>> mockInterchangeEventLoad dependenciesFetched
                >>> runAssessDependenciesRet assessDependenciesResult
                >>> runGenerateReportRet generateReportResult
                >>> runPublishComponentResultRet @RunResult
                >>> runIdentity

        let handlerResult = reportGeneratorHandler "asdf/asdf" receivedMessage
            (publishedReportsGenerated, (writtenGenerateReportRequests, (writtenAssessDependenciesRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, _)))))) = runHandler handlerResult

        annotate "wrote a ReportGenerated queue event"
        genericLength @Integer publishedReportsGenerated === 1

        annotate "emitted app events"
        genericLength @Integer appEvents === 1

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

        annotate "assesses dependencies"
        genericLength @Integer writtenAssessDependenciesRequests === 1

        annotate "generates a report"
        genericLength @Integer writtenGenerateReportRequests === 1
