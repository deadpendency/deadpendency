{-# LANGUAGE DataKinds #-}

module EP.Handler.PublishFailedReportHandler
  ( publishFailedReportHandler,
    PublishFailedReportRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult
import Common.Model.Error.ProcessingError
import Control.Algebra (Has)
import EP.Model.AlwaysJSON
import Network.Google.PubSub qualified as G
import Servant (JSON, NoContent (..), Post, ReqBody, (:>))

type PublishFailedReportRoute =
  ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

publishFailedReportHandler ::
  ( Has AppEventEmit sig m,
    Has (InterchangeEventLoad AlwaysJSON) sig m,
    Has (PublishComponentResult AlwaysJSON) sig m
  ) =>
  G.ReceivedMessage ->
  m NoContent
publishFailedReportHandler receivedMessage = do
  emitAppEventInfo (AppEventMessage "Received Publish Failed Report")

  interchangeEvent <- loadInterchangeEvent @AlwaysJSON receivedMessage

  -- known failures will produce a normal report, so do not need the unexpected error report
  if interchangeEvent ^. #_knownFailureOccurred
    then pure ()
    else publishComponentResult @AlwaysJSON $ FailureComponentResult ProcessingErrorApplication

  emitAppEventInfo (AppEventMessage "Finished Publish Failed Report")
  pure NoContent
