{-# LANGUAGE DataKinds #-}

module SR.Effect.StreamQueueMessages.Carrier.StreamQueueMessagesGooglePubSubC
  ( StreamQueueMessagesGooglePubSubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Conduit ((.|))
import Conduit qualified as C
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Data.Conduit.Combinators qualified as C
import GHC.TypeLits (Symbol)
import Network.Google qualified as G
import Network.Google.Auth.Scope qualified as G
import Network.Google.PubSub.Types qualified as G
import Network.Google.Resource.PubSub.Projects.Subscriptions.Acknowledge qualified as G
import Network.Google.Resource.PubSub.Projects.Subscriptions.Pull qualified as G
import Network.Google.Resource.PubSub.Projects.Topics.Publish qualified as G
import SR.Effect.StreamQueueMessages.Model.StreamQueueMessagesRequest
import SR.Effect.StreamQueueMessages.StreamQueueMessages

newtype StreamQueueMessagesGooglePubSubIOC (s :: [Symbol]) m a = StreamQueueMessagesGooglePubSubIOC {runStreamQueueMessagesGooglePubSubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Reader (G.Env s)) sig m,
    G.AllowScopes s,
    G.HasScope' s PubSubRequiredScopes ~ 'True,
    MonadIO m
  ) =>
  Algebra (StreamQueueMessages :+: sig) (StreamQueueMessagesGooglePubSubIOC s m)
  where
  alg hdl sig ctx = case sig of
    (L (StreamQueueMessages (StreamQueueMessagesRequest dlqSubId mainTopic))) -> do
      emitAppEventInfo (AppEventMessage $ "Started: Replay Queue Messages to " <> mainTopic <> " from " <> dlqSubId)
      env <- ask @(G.Env s)
      let fullSubId = "projects/dgtw-deadpendency-action-2/subscriptions/" <> dlqSubId
      streamMessages env fullSubId mainTopic

      emitAppEventInfo (AppEventMessage $ "Finished: Replay Queue Messages to " <> mainTopic <> " from " <> dlqSubId)

      StreamQueueMessagesGooglePubSubIOC $ pure ctx
    (R other) -> StreamQueueMessagesGooglePubSubIOC $ alg (runStreamQueueMessagesGooglePubSubIOC . hdl) other ctx

streamMessages ::
  (G.AllowScopes s, G.HasScope' s PubSubRequiredScopes ~ 'True, MonadIO m, Has AppEventEmit sig m) =>
  G.Env s ->
  Text ->
  Text ->
  m ()
streamMessages env withSubId toTopicId =
  C.runConduit $
    allMessagesStream env withSubId
      .| C.iterM (ackMessages env withSubId)
      .| C.iterM (writeMessagesToRegularTopic env toTopicId)
      .| C.sinkNull

-- can this instead use `yieldMany` and use some kind of batching mechanism? tried but failed
allMessagesStream :: (G.AllowScopes s, G.HasScope' s PubSubRequiredScopes ~ 'True, MonadIO m, Has AppEventEmit sig m) => G.Env s -> Text -> C.ConduitT () [G.ReceivedMessage] m ()
allMessagesStream env subId = do
  lift $ emitAppEventInfo (AppEventMessage "Fetch messages")
  messages <- liftIO $ pollForMessages env subId
  lift $ emitAppEventInfo (AppEventMessage $ "Fetched message count " <> show (genericLength @Int messages))
  case messages of
    [] -> pure ()
    xs -> C.yield xs *> allMessagesStream env subId

writeMessagesToRegularTopic ::
  (G.AllowScopes s, G.HasScope' s PubSubRequiredScopes ~ 'True, MonadIO m, Has AppEventEmit sig m) =>
  G.Env s ->
  Text ->
  [G.ReceivedMessage] ->
  m ()
writeMessagesToRegularTopic env topicId messages = do
  emitAppEventInfo (AppEventMessage "Write messages")
  let request = createPublishRequest topicId messages
  result <- liftIO $ (G.runResourceT . G.runGoogle env . G.send) request
  emitAppEventInfoA (AppEventMessage "Wrote messages") (AppEventAdditional result)

ackMessages ::
  (G.AllowScopes s, G.HasScope' s PubSubRequiredScopes ~ 'True, MonadIO m, Has AppEventEmit sig m) =>
  G.Env s ->
  Text ->
  [G.ReceivedMessage] ->
  m ()
ackMessages env subId messages = do
  emitAppEventInfo (AppEventMessage "Ack messages")
  let request = createAckRequest subId messages
  result <- liftIO $ (G.runResourceT . G.runGoogle env . G.send) request
  emitAppEventInfoA (AppEventMessage "Acked messages") (AppEventAdditional result)

pollForMessages :: (G.AllowScopes s, G.HasScope' s PubSubRequiredScopes ~ 'True) => G.Env s -> Text -> IO [G.ReceivedMessage]
pollForMessages env subId =
  let request = G.projectsSubscriptionsPull createPullRequest subId
      result = (G.runResourceT . G.runGoogle env . G.send) request
   in result <&> \r -> r ^. G.prReceivedMessages

createPullRequest :: G.PullRequest
createPullRequest =
  G.pullRequest
    & G.prMaxMessages ?~ 50

createPublishRequest :: Text -> [G.ReceivedMessage] -> G.ProjectsTopicsPublish
createPublishRequest pubsubQueueTopicId messages = G.projectsTopicsPublish request pubsubQueueTopicId
  where
    messagesData =
      case for messages (\m -> m ^. (G.rmMessage . _Just . G.pmData)) of
        Just result -> result
        Nothing -> error "unexpected message missing data"
    asPubsubMessages =
      messagesData <&> \md ->
        G.pubsubMessage
          & G.pmData ?~ md
    request = G.publishRequest & G.prMessages .~ asPubsubMessages

createAckRequest :: Text -> [G.ReceivedMessage] -> G.ProjectsSubscriptionsAcknowledge
createAckRequest subId messages = G.projectsSubscriptionsAcknowledge request subId
  where
    ackIds =
      case for messages (^. G.rmAckId) of
        Just result -> result
        Nothing -> error "unexpected message missing ack ids"
    request = G.acknowledgeRequest & G.arAckIds .~ ackIds

type PubSubRequiredScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform",
     "https://www.googleapis.com/auth/pubsub"
   ]
