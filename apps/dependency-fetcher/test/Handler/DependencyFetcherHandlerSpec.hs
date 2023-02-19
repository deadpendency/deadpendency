module Handler.DependencyFetcherHandlerSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadMockC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultRetC
import Common.Effect.PublishFailedMessage.Carrier.PublishFailedMessageRetC
import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Common.Effect.Trace.Carrier.TraceEmitRetC
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.InterchangeEvent.DependenciesDetermined
import Common.Model.InterchangeEvent.DependenciesFetched
import CommonTest.Gen.Effect.TraceEmit
import CommonTest.Gen.Gogol.PubSub
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.InterchangeEvent
import Control.Carrier.Error.Either (runError)
import DF.Effect.FetchDependencies.Carrier.FetchDependenciesRetC
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Handler.DependencyFetcherHandler
import Data.Vector qualified as V
import Gen.Effect.FetchDependencies
import Hedgehog.Gen (sample)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "When processing events" $ do
    it "a happy day test produces correct results" $
      hedgehog $ do
        dependenciesDetermined <- forAll (genInterchangeEvent genDependenciesDetermined)
        fetchDependenciesResult <- forAll genFetchDependenciesResult
        receivedMessage <- sample genPubSubReceivedMessage
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let runHandler =
              runError @FetchDependenciesError
                >>> runTraceEmitRet runTrace startSpanResult
                >>> runAppEventEmitRet
                >>> mockInterchangeEventLoad dependenciesDetermined
                >>> runFetchDependenciesRet fetchDependenciesResult
                >>> runPublishComponentResultRet @DependenciesFetched
                >>> runPublishFailedMessageRet @DependenciesDetermined
                >>> runIdentity

        let handlerResult = dependencyFetcherHandler "asdf/asdf" receivedMessage
            (_, (publishedDependenciesFetched, (writtenFetchDepsRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, _)))))) = runHandler handlerResult

        annotate "wrote a DependenciesFetched queue event"
        genericLength @Integer publishedDependenciesFetched === 1

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        annotate "writes a write checks request"
        genericLength @Integer writtenFetchDepsRequests === 1

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

    it "a unexpected parse failure produces a failed message publish" $
      hedgehog $ do
        dependenciesDetermined <- forAll (genInterchangeEvent genDependenciesDetermined)
        fetchDependenciesResult <- forAll genFetchDependenciesResult
        receivedMessage <- sample genPubSubReceivedMessage
        erroredDependency <- sample genErroredDependency
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let massagedFetchDepsResult =
              fetchDependenciesResult
                & (#_erroredRepoDependencies . #_dependencies)
                  .~ V.singleton (erroredDependency & #_erroredReason .~ UnexpectedFailureToParseRegistryEntry "blah")

        let runHandler =
              runIdentity
                . runPublishFailedMessageRet @DependenciesDetermined
                . runPublishComponentResultRet @DependenciesFetched
                . runFetchDependenciesRet massagedFetchDepsResult
                . mockInterchangeEventLoad dependenciesDetermined
                . runAppEventEmitRet
                . runTraceEmitRet runTrace startSpanResult
                . runError @FetchDependenciesError

        let handlerResult = dependencyFetcherHandler "asdf/asdf" receivedMessage
            (publishedFailedMessages, (publishedDependenciesFetched, (writtenFetchDepsRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, _)))))) = runHandler handlerResult

        annotate "wrote the failed queue message"
        publishedFailedMessages === [FailedInterchangeEvent dependenciesDetermined]

        annotate "wrote a DependenciesFetched queue event"
        genericLength @Integer publishedDependenciesFetched === 1

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        annotate "writes a write checks request"
        genericLength @Integer writtenFetchDepsRequests === 1

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

    it "a unexpected data inconsistent failure produces a failed message publish" $
      hedgehog $ do
        dependenciesDetermined <- forAll (genInterchangeEvent genDependenciesDetermined)
        fetchDependenciesResult <- forAll genFetchDependenciesResult
        receivedMessage <- sample genPubSubReceivedMessage
        erroredDependency <- sample genErroredDependency
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let massagedFetchDepsResult =
              fetchDependenciesResult
                & (#_erroredRepoDependencies . #_dependencies)
                  .~ V.singleton (erroredDependency & #_erroredReason .~ UnexpectedFailureRegistryDataInconsistent "blah")

        let runHandler =
              runIdentity
                . runPublishFailedMessageRet @DependenciesDetermined
                . runPublishComponentResultRet @DependenciesFetched
                . runFetchDependenciesRet massagedFetchDepsResult
                . mockInterchangeEventLoad dependenciesDetermined
                . runAppEventEmitRet
                . runTraceEmitRet runTrace startSpanResult
                . runError @FetchDependenciesError

        let handlerResult = dependencyFetcherHandler "asdf/asdf" receivedMessage
            (publishedFailedMessages, (publishedDependenciesFetched, (writtenFetchDepsRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, _)))))) = runHandler handlerResult

        annotate "wrote the failed queue message"
        publishedFailedMessages === [FailedInterchangeEvent dependenciesDetermined]

        annotate "wrote a DependenciesFetched queue event"
        genericLength @Integer publishedDependenciesFetched === 1

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        annotate "writes a write checks request"
        genericLength @Integer writtenFetchDepsRequests === 1

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1
