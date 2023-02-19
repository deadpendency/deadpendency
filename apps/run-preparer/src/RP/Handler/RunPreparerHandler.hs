{-# LANGUAGE DataKinds #-}

module RP.Handler.RunPreparerHandler
  ( runPreparerHandler,
    RunPreparerRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.InterchangeEvent.CheckRunCreated
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunPrepared
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Has)
import Control.Effect.Catch (Catch)
import Control.Effect.State (State, put)
import Network.Google.PubSub qualified as G
import RP.Effect.ReadConfig.Model.ReadConfigError
import RP.Effect.ReadConfig.Model.ReadConfigRequest
import RP.Effect.ReadConfig.Model.ReadConfigResult
import RP.Effect.ReadConfig.ReadConfig
import RP.Effect.VerifyPlan.VerifyPlan
import Servant.API

type RunPreparerRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

runPreparerHandler ::
  ( Has AppEventEmit sig m,
    Has (Catch ReadConfigError) sig m,
    Has TraceEmit sig m,
    Has (State (Maybe RepoConfig)) sig m,
    Has (PublishComponentResult RunPrepared) sig m,
    Has (InterchangeEventLoad CheckRunCreated) sig m,
    Has ReadConfig sig m,
    Has VerifyPlan sig m
  ) =>
  Text ->
  G.ReceivedMessage ->
  m NoContent
runPreparerHandler googleTraceId receivedMessage = do
  (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Run Prepared")

  emitAppEventInfo $ AppEventMessage "Receive raw message"

  -- decode queue event and log and put in state
  interchangeEvent <- loadInterchangeEvent @CheckRunCreated receivedMessage

  emitAppEventInfoA (AppEventMessage "Receive run-prepared") (AppEventAdditional interchangeEvent)

  -- in the future this will throw a processing failure
  planVerify

  let programmingLanguages = interchangeEvent ^. (#_result . #_repoProgrammingLanguages)

      -- retrieve config
      readConfigRequest = createReadConfigRequest interchangeEvent
      mReadConfigResult = configRead readConfigRequest

  componentResult <-
    handleError @ReadConfigError
      mReadConfigResult
      \(ReadConfigResult config) -> do
        -- the run is pulled from state for queue publishing as well, so we need to update it with the config here
        put $ Just config
        pure $ SuccessComponentResult $ RunPrepared programmingLanguages

  -- finish up
  publishComponentResult componentResult
  concludeSpan (ConcludeSpanRequest span)
  pure NoContent

createReadConfigRequest :: InterchangeEvent CheckRunCreated -> ReadConfigRequest
createReadConfigRequest riCheckRunCreated =
  ReadConfigRequest
    { _gitSha = headSha,
      _gHQualifiedRepo = qualifiedRepo
    }
  where
    run = riCheckRunCreated ^. #_run
    headSha = run ^. #_gitHeadSha
    qualifiedRepo = run ^. #_qualifiedRepo
