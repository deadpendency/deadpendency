{-# LANGUAGE DataKinds #-}

module Common.Effect.QueueEventPublish.Carrier.QueueEventPublishGooglePubSubC
  ( QueueEventPublishGooglePubSubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.QueueEventPublish.Model.QueueEventPublishRequest
import Common.Effect.QueueEventPublish.Model.QueuePayload
import Common.Effect.QueueEventPublish.Model.QueueTopicId
import Common.Effect.QueueEventPublish.QueueEventPublish
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Data.Aeson
import Data.ByteString.Base64 qualified as B64
import GHC.TypeLits (Symbol)
import Network.Google qualified as G
import Network.Google.Auth.Scope qualified as G
import Network.Google.PubSub.Types qualified as G
import Network.Google.Resource.PubSub.Projects.Topics.Publish qualified as G

newtype QueueEventPublishGooglePubSubIOC (s :: [Symbol]) m a = QueueEventPublishGooglePubSubIOC {runQueueEventPublishGooglePubSubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Reader (G.Env s)) sig m,
    G.AllowScopes s,
    G.HasScope' s PubSubRequiredScopes ~ 'True,
    MonadIO m
  ) =>
  Algebra (QueueEventPublish :+: sig) (QueueEventPublishGooglePubSubIOC s m)
  where
  alg hdl sig ctx = case sig of
    (L (PublishQueueEvent (QueueEventPublishRequest (QueueTopicId topicId) queuePayload))) -> do
      emitAppEventInfo (AppEventMessage "Started: Publish PubSub Message")
      let encodedPayload =
            case queuePayload of
              QueuePayload toJsonPayload -> B64.encodeBase64' $ fromLazy $ encode toJsonPayload
              RawQueuePayload bsQueuePayload -> bsQueuePayload

      env <- ask @(G.Env s)
      let publishRequest = createPublishRequest topicId encodedPayload
          innerLogic = (G.runResourceT . G.runGoogle env . G.send) publishRequest
      liftIO innerLogic
        >>= \response ->
          emitAppEventInfoA (AppEventMessage "Finished: Publish PubSub Message") (AppEventAdditional response)
            $> ctx
    (R other) -> QueueEventPublishGooglePubSubIOC $ alg (runQueueEventPublishGooglePubSubIOC . hdl) other ctx

createPublishRequest :: Text -> ByteString -> G.ProjectsTopicsPublish
createPublishRequest pubsubQueueTopicId message = G.projectsTopicsPublish request pubsubQueueTopicId
  where
    request = G.publishRequest & G.prMessages .~ messages

    messages =
      [ G.pubsubMessage & G.pmData ?~ message
      ]

type PubSubRequiredScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform",
     "https://www.googleapis.com/auth/pubsub"
   ]
