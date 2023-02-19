{-# LANGUAGE DataKinds #-}

module DF.Handler.DependencyFetcherHandler
  ( dependencyFetcherHandler,
    DependencyFetcherRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad (InterchangeEventLoad (..), loadInterchangeEvent)
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult
import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Common.Effect.PublishFailedMessage.PublishFailedMessage
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.InterchangeEvent.DependenciesDetermined
import Common.Model.InterchangeEvent.DependenciesFetched
import Control.Algebra (Has)
import Control.Effect.Catch (Catch)
import DF.Effect.FetchDependencies.FetchDependencies
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchDependencies.Model.FetchDependenciesRequest
import DF.Effect.FetchDependencies.Model.FetchDependenciesResult
import Network.Google.PubSub qualified as G
import Servant.API

type DependencyFetcherRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

dependencyFetcherHandler ::
  ( Has AppEventEmit sig m,
    Has TraceEmit sig m,
    Has (Catch FetchDependenciesError) sig m,
    Has (PublishComponentResult DependenciesFetched) sig m,
    Has (InterchangeEventLoad DependenciesDetermined) sig m,
    Has FetchDependencies sig m,
    Has (PublishFailedMessage DependenciesDetermined) sig m
  ) =>
  Text ->
  G.ReceivedMessage ->
  m NoContent
dependencyFetcherHandler googleTraceId receivedMessage = do
  (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Dependency Fetcher")

  emitAppEventInfo $ AppEventMessage "Receive raw message"

  -- decode queue event
  interchangeEvent <- loadInterchangeEvent @DependenciesDetermined receivedMessage
  let dependenciesDetermined = interchangeEvent ^. #_result

  emitAppEventInfoA (AppEventMessage "Receive dependency fetcher") (AppEventAdditional interchangeEvent)

  -- do the work
  let fetchDepsRequest = createFetchDependenciesRequest dependenciesDetermined
      mFetchDepsResult = fetchDependencies fetchDepsRequest
  componentResult <-
    handleError @FetchDependenciesError
      mFetchDepsResult
      \fetchDepsResult -> do
        -- if we have a graceful failure, start by publishing the recieved message to the dlq
        when
          (gracefulFailureInResult fetchDepsResult)
          (publishFailedMessage $ FailedInterchangeEvent interchangeEvent)

        let dependenciesFetched = createDependenciesFetched dependenciesDetermined fetchDepsResult
        pure $ SuccessComponentResult dependenciesFetched

  -- publish result to the queue
  publishComponentResult componentResult

  -- finish up
  concludeSpan (ConcludeSpanRequest span)
  pure NoContent

gracefulFailureInResult :: FetchDependenciesResult -> Bool
gracefulFailureInResult =
  anyOf
    (#_erroredRepoDependencies . #_dependencies . folded . #_erroredReason)
    ( \case
        UnexpectedFailureToParseRegistryEntry _ -> True
        UnexpectedFailureRegistryDataInconsistent _ -> True
        UnexpectedDependencyNameInvalid _ -> True
        _ -> False
    )

createFetchDependenciesRequest :: DependenciesDetermined -> FetchDependenciesRequest
createFetchDependenciesRequest dependenciesDetermined =
  let repoDependencies = dependenciesDetermined ^. #_basicRepoDependencies
   in FetchDependenciesRequest
        { _repositoryDependencies = repoDependencies
        }

createDependenciesFetched :: DependenciesDetermined -> FetchDependenciesResult -> DependenciesFetched
createDependenciesFetched dependenciesDetermined fetchDependenciesResult =
  let enrichedDeps = fetchDependenciesResult ^. #_enrichedRepoDependencies
      failedDeps = fetchDependenciesResult ^. #_erroredRepoDependencies
      ignoredDeps = dependenciesDetermined ^. #_ignoredRepoDependencies
   in DependenciesFetched
        { _enrichedRepoDependencies = enrichedDeps,
          _erroredRepoDependencies = failedDeps,
          _ignoredRepoDependencies = ignoredDeps
        }
