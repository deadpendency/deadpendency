module Common.Effect.QueueEventPublish.Model.QueuePayload
  ( QueuePayload (..),
  )
where

import Data.Aeson

data QueuePayload where
  QueuePayload :: (ToJSON a) => a -> QueuePayload
  RawQueuePayload :: ByteString -> QueuePayload
