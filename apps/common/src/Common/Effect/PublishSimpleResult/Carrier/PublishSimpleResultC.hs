module Common.Effect.PublishSimpleResult.Carrier.PublishSimpleResultC
  ( PublishSimpleResultIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.PublishSimpleResult.Model.SimpleResult
import Common.Effect.PublishSimpleResult.PublishSimpleResult (PublishSimpleResult (..))
import Common.Effect.QueueEventPublish.Model.QueueEventPublishRequest
import Common.Effect.QueueEventPublish.Model.QueuePayload
import Common.Effect.QueueEventPublish.Model.QueueTopicId
import Common.Effect.QueueEventPublish.QueueEventPublish
import Common.Model.Config.CommonConfig
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Throw (Throw, liftEither)

newtype PublishSimpleResultIOC (p :: Type) m a = PublishSimpleResultIOC {runPublishSimpleResultIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Reader CommonConfig) sig m,
    Has (Throw CommonError) sig m,
    Has QueueEventPublish sig m,
    MonadIO m
  ) =>
  Algebra (PublishSimpleResult p :+: sig) (PublishSimpleResultIOC p m)
  where
  alg hdl sig ctx = case sig of
    (L (PublishSimpleResult (SimpleResult p))) -> do
      emitAppEventInfo (AppEventMessage "Started: Publish Simple Result")
      commonConfig <- ask @CommonConfig
      pubsubQueueTopicId <- (liftEither . maybeToRight PubSubPublishQueueIdConfigMissing . _downstreamPubSubQueueId) commonConfig

      let request =
            QueueEventPublishRequest
              { _topicId = QueueTopicId pubsubQueueTopicId,
                _payload = QueuePayload p
              }

      publishQueueEvent request

      emitAppEventInfo (AppEventMessage "Finished: Publish Simple Result")
      PublishSimpleResultIOC $ pure ctx
    (R other) -> PublishSimpleResultIOC $ alg (runPublishSimpleResultIOC . hdl) other ctx
