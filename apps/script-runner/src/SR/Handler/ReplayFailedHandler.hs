{-# LANGUAGE DataKinds #-}

module SR.Handler.ReplayFailedHandler
  ( replayFailedHandler,
    ReplayFailedRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Model.Config.AppEnv
import Common.Model.Config.CommonConfig
import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask)
import SR.Effect.StreamQueueMessages.Model.StreamQueueMessagesRequest
import SR.Effect.StreamQueueMessages.StreamQueueMessages
import SR.Model.Config
import Servant (JSON, NoContent (..), Post, (:>))

type ReplayFailedRoute =
  "replay-failed"
    :> Post '[JSON] NoContent

replayFailedHandler ::
  ( Has AppEventEmit sig m,
    Has StreamQueueMessages sig m,
    Has (Reader Config) sig m,
    Has (Reader CommonConfig) sig m
  ) =>
  m NoContent
replayFailedHandler = do
  emitAppEventInfo (AppEventMessage "Received Message Replay")

  config <- ask @Config
  commonConfig <- ask @CommonConfig

  streamQueueMessages
    StreamQueueMessagesRequest
      { _pullDlqSubscriptionId = config ^. #_reportGeneratedQueueDlqSubIdA,
        _pullMainTopic = config ^. #_reportGeneratedQueueTopicId
      }

  streamQueueMessages
    StreamQueueMessagesRequest
      { _pullDlqSubscriptionId = config ^. #_depsFetchedQueueDlqSubIdA,
        _pullMainTopic = config ^. #_depsFetchedQueueTopicId
      }

  streamQueueMessages
    StreamQueueMessagesRequest
      { _pullDlqSubscriptionId = config ^. #_depsDeterminedQueueDlqSubIdA,
        _pullMainTopic = config ^. #_depsDeterminedQueueTopicId
      }

  streamQueueMessages
    StreamQueueMessagesRequest
      { _pullDlqSubscriptionId = config ^. #_runPreparedQueueDlqSubIdA,
        _pullMainTopic = config ^. #_runPreparedQueueTopicId
      }

  streamQueueMessages
    StreamQueueMessagesRequest
      { _pullDlqSubscriptionId = config ^. #_checkRunCreatedQueueDlqSubIdA,
        _pullMainTopic = config ^. #_checkRunCreatedQueueTopicId
      }

  streamQueueMessages
    StreamQueueMessagesRequest
      { _pullDlqSubscriptionId = config ^. #_initQueueDlqSubIdA,
        _pullMainTopic = config ^. #_initQueueTopicId
      }

  when
    (commonConfig ^. #_appEnv == Prod)
    do
      streamQueueMessages
        StreamQueueMessagesRequest
          { _pullDlqSubscriptionId = config ^. #_reportGeneratedQueueDlqSubIdB,
            _pullMainTopic = config ^. #_reportGeneratedQueueTopicId
          }

      streamQueueMessages
        StreamQueueMessagesRequest
          { _pullDlqSubscriptionId = config ^. #_depsFetchedQueueDlqSubIdB,
            _pullMainTopic = config ^. #_depsFetchedQueueTopicId
          }

      streamQueueMessages
        StreamQueueMessagesRequest
          { _pullDlqSubscriptionId = config ^. #_depsDeterminedQueueDlqSubIdB,
            _pullMainTopic = config ^. #_depsDeterminedQueueTopicId
          }

      streamQueueMessages
        StreamQueueMessagesRequest
          { _pullDlqSubscriptionId = config ^. #_runPreparedQueueDlqSubIdB,
            _pullMainTopic = config ^. #_runPreparedQueueTopicId
          }

      streamQueueMessages
        StreamQueueMessagesRequest
          { _pullDlqSubscriptionId = config ^. #_checkRunCreatedQueueDlqSubIdB,
            _pullMainTopic = config ^. #_checkRunCreatedQueueTopicId
          }

      streamQueueMessages
        StreamQueueMessagesRequest
          { _pullDlqSubscriptionId = config ^. #_initQueueDlqSubIdB,
            _pullMainTopic = config ^. #_initQueueTopicId
          }

  emitAppEventInfo (AppEventMessage "Finished Message Replay")
  pure NoContent
