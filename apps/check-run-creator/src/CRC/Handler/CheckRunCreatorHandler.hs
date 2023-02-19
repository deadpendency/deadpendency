{-# LANGUAGE DataKinds #-}

module CRC.Handler.CheckRunCreatorHandler
  ( checkRunCreatorHandler,
    CheckRunCreatorRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import Common.Effect.GitHub.WriteChecks.WriteChecks
import Common.Effect.InterchangeEventDecode.InterchangeEventDecode
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.InterchangeEvent.CheckRunCreated
import Common.Model.InterchangeEvent.RunCreated
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Has)
import Control.Effect.State (State, put)
import Network.Google.PubSub qualified as G
import Servant.API

type CheckRunCreatorRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

checkRunCreatorHandler ::
  ( Has AppEventEmit sig m,
    Has (State (Maybe RunTrace)) sig m,
    Has (State (Maybe Run)) sig m,
    Has (State (Maybe CheckRun)) sig m,
    Has (State (Maybe RepoConfig)) sig m,
    Has TraceEmit sig m,
    Has (PublishComponentResult CheckRunCreated) sig m,
    Has (InterchangeEventDecode RunCreated) sig m,
    Has WriteChecks sig m
  ) =>
  Text ->
  G.ReceivedMessage ->
  m NoContent
checkRunCreatorHandler googleTraceId receivedMessage = do
  (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Check Run Creator")

  emitAppEventInfo $ AppEventMessage "Receive raw message"

  -- decode queue event and log and put in state
  runCreated <- decodeInterchangeEvent receivedMessage

  -- update state with run details
  let run = runCreated ^. #_run
      runTrace = run ^. #_runTrace
  put $ Just run
  put $ Just runTrace
  -- we start with a default config
  put $ Just defaultRepoConfig

  emitAppEventInfoA (AppEventMessage "Receive run-created") (AppEventAdditional runCreated)

  -- create check run
  let checkRunRequest = createCheckRunRequest runCreated
  checkRunRequestResult <- createCheckRun checkRunRequest

  put $ Just $ checkRunRequestResult ^. #_checkRun

  let componentResult =
        SuccessComponentResult $ produceCheckRunCreatedEvent checkRunRequestResult

  -- finish up
  publishComponentResult componentResult
  concludeSpan (ConcludeSpanRequest span)
  pure NoContent

produceCheckRunCreatedEvent :: CheckRunCreateResult -> CheckRunCreated
produceCheckRunCreatedEvent crrResult =
  CheckRunCreated
    { _repoProgrammingLanguages = programmingLanguages
    }
  where
    programmingLanguages = crrResult ^. #_repoProgrammingLanguages

createCheckRunRequest :: RunCreated -> CheckRunCreateRequest
createCheckRunRequest runCreated =
  CheckRunCreateRequest
    { _headSha = headSha,
      _name = name,
      _repoNodeId = repoNodeId,
      _appInstallationId = installId
    }
  where
    run = runCreated ^. #_run
    headSha = run ^. #_gitHeadSha
    name = "Deadpendency Check"
    repoNodeId = run ^. #_repoNodeId
    installId = run ^. #_appInstallationId
