module Common.Effect.PublishComponentResult.Carrier.PublishComponentResultC
  ( PublishComponentResultIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult (PublishComponentResult (..))
import Common.Effect.QueueEventPublish.Model.QueueEventPublishRequest
import Common.Effect.QueueEventPublish.Model.QueuePayload
import Common.Effect.QueueEventPublish.Model.QueueTopicId
import Common.Effect.QueueEventPublish.QueueEventPublish
import Common.Effect.Util
import Common.Model.Config.CommonConfig
import Common.Model.Details.Run
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunResult
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Control.Effect.State (State)
import Control.Effect.Throw (Throw, liftEither)

newtype PublishComponentResultIOC (p :: Type) m a = PublishComponentResultIOC {runPublishComponentResultIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Reader CommonConfig) sig m,
    Has (Throw CommonError) sig m,
    Has (State (Maybe Run)) sig m,
    Has (State (Maybe RepoConfig)) sig m,
    Has (State (Maybe CheckRun)) sig m,
    Has (State (Maybe GHInstallationAuth)) sig m,
    Has QueueEventPublish sig m,
    MonadIO m
  ) =>
  Algebra (PublishComponentResult p :+: sig) (PublishComponentResultIOC p m)
  where
  alg hdl sig ctx = case sig of
    (L (PublishComponentResult queueEvent)) -> do
      emitAppEventInfo (AppEventMessage "Started: Publish Component Result")
      commonConfig <- ask @CommonConfig
      installAuth <- getInstallAuth
      run <- getRun
      repoConfig <- getRepoConfig
      checkRun <- getCheckRun
      (pubsubQueueTopicId, queuePayload) <-
        case queueEvent of
          (SuccessComponentResult p) -> do
            emitAppEventInfo (AppEventMessage "During: Publish PubSub Message - Publish Success")
            pubsubQueueTopicId' <- (liftEither . maybeToRight PubSubPublishQueueIdConfigMissing . _downstreamPubSubQueueId) commonConfig
            pure (pubsubQueueTopicId', QueuePayload $ InterchangeEvent installAuth checkRun run repoConfig False p)
          (FailureComponentResult f) -> do
            emitAppEventInfo (AppEventMessage "During: Publish PubSub Message - Publish Failure")
            pubsubQueueTopicId' <- (liftEither . maybeToRight PubSubFailureQueueIdConfigMissing . _failurePubSubQueueId) commonConfig
            pure (pubsubQueueTopicId', QueuePayload $ InterchangeEvent installAuth checkRun run repoConfig True $ RunFailure f)

      let request =
            QueueEventPublishRequest
              { _topicId = QueueTopicId pubsubQueueTopicId,
                _payload = queuePayload
              }

      publishQueueEvent request

      emitAppEventInfo (AppEventMessage "Finished: Publish Component Result")
      PublishComponentResultIOC $ pure ctx
    (R other) -> PublishComponentResultIOC $ alg (runPublishComponentResultIOC . hdl) other ctx
