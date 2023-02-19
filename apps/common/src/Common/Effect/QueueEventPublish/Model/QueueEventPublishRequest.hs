module Common.Effect.QueueEventPublish.Model.QueueEventPublishRequest
  ( QueueEventPublishRequest (..),
  )
where

import Common.Effect.QueueEventPublish.Model.QueuePayload
import Common.Effect.QueueEventPublish.Model.QueueTopicId

data QueueEventPublishRequest = QueueEventPublishRequest
  { _topicId :: QueueTopicId,
    _payload :: QueuePayload
  }
