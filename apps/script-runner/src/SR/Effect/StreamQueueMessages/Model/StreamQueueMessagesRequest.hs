module SR.Effect.StreamQueueMessages.Model.StreamQueueMessagesRequest
  ( StreamQueueMessagesRequest (..),
  )
where

data StreamQueueMessagesRequest = StreamQueueMessagesRequest
  { _pullDlqSubscriptionId :: Text,
    _pullMainTopic :: Text
  }
  deriving stock (Eq, Show, Generic)
