module Common.Effect.PublishFailedMessage.Carrier.PublishFailedMessageC
  ( PublishFailedMessageIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Common.Effect.PublishFailedMessage.PublishFailedMessage (PublishFailedMessage (..))
import Common.Effect.QueueEventPublish.Model.QueueEventPublishRequest
import Common.Effect.QueueEventPublish.Model.QueuePayload
import Common.Effect.QueueEventPublish.Model.QueueTopicId
import Common.Effect.QueueEventPublish.QueueEventPublish
import Common.Effect.Util
import Common.Model.Config.CommonConfig
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Throw (Throw)

newtype PublishFailedMessageIOC p m a = PublishFailedMessageIOC {runPublishFailedMessageIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Reader CommonConfig) sig m,
    Has (Throw CommonError) sig m,
    Has QueueEventPublish sig m,
    MonadIO m
  ) =>
  Algebra (PublishFailedMessage p :+: sig) (PublishFailedMessageIOC p m)
  where
  alg hdl sig ctx = case sig of
    (L (PublishFailedMessage (FailedInterchangeEvent interchangeEvent))) -> do
      emitAppEventInfo (AppEventMessage "Started: Publish Failed Message")
      commonConfig <- ask @CommonConfig
      dlqQueueTopicId <- (maybeToErrorM PubSubDlqQueueIdConfigMissing . _dlqPubSubQueueId) commonConfig
      let setKnownFailureIE = interchangeEvent & #_knownFailureOccurred .~ True

      let request =
            QueueEventPublishRequest
              { _topicId = QueueTopicId dlqQueueTopicId,
                _payload = QueuePayload setKnownFailureIE
              }

      publishQueueEvent request

      emitAppEventInfo (AppEventMessage "Finished: Publish Failed Message")
      PublishFailedMessageIOC $ pure ctx
    (R other) -> PublishFailedMessageIOC $ alg (runPublishFailedMessageIOC . hdl) other ctx
