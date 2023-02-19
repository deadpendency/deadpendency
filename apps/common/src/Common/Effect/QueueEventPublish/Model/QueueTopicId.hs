module Common.Effect.QueueEventPublish.Model.QueueTopicId
  ( QueueTopicId (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype QueueTopicId = QueueTopicId
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON QueueTopicId where
  toJSON = genericToJSON cleanJSONOptions
