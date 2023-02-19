{-# LANGUAGE DataKinds #-}

module RF.Handler.RunFinalizerHandler
  ( runFinalizerHandler,
    RunFinalizerRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad (InterchangeEventLoad (..), loadInterchangeEvent)
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunResult
import Common.Model.Report.DependencyReports
import Control.Algebra (Has)
import Data.Vector qualified as V
import Network.Google.PubSub qualified as G
import RF.Effect.FinishRun.FinishRun
import RF.Effect.FinishRun.Model.FinishRunRequest
import Servant.API

type RunFinalizerRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

runFinalizerHandler ::
  ( Has AppEventEmit sig m,
    Has (InterchangeEventLoad RunResult) sig m,
    Has TraceEmit sig m,
    Has FinishRun sig m
  ) =>
  Text ->
  G.ReceivedMessage ->
  m NoContent
runFinalizerHandler googleTraceId receivedMessage = do
  (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Run Finalizer")

  emitAppEventInfo $ AppEventMessage "Receive raw message"

  -- decode queue event
  interchangeEvent <- loadInterchangeEvent receivedMessage
  let reportGenerated = interchangeEvent ^. #_result

  emitAppEventInfoA (AppEventMessage "Receive run finalizer") (AppEventAdditional $ trimRunResult interchangeEvent)

  -- do the work
  let request = createFinishRunRequest reportGenerated
  finishRun request

  -- finish up
  concludeSpan (ConcludeSpanRequest span)
  pure NoContent

createFinishRunRequest :: RunResult -> FinishRunRequest
createFinishRunRequest runResult =
  FinishRunRequest
    { _runResult = runResult
    }

-- a quick hack to avoid excessive logging when there are too many reports to log
-- this will be solved with a typeclass solution soon tm
trimRunResult :: InterchangeEvent RunResult -> InterchangeEvent RunResult
trimRunResult input =
  input
    & (#_result . _Ctor @"RunSuccess" . #_dependencyReports) .~ DRSingleLanguageReports V.empty V.empty V.empty V.empty V.empty
