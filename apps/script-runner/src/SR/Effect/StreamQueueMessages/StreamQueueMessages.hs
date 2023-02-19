{-# LANGUAGE TemplateHaskell #-}

module SR.Effect.StreamQueueMessages.StreamQueueMessages
  ( StreamQueueMessages (..),
    streamQueueMessages,
  )
where

import Control.Effect.TH
import SR.Effect.StreamQueueMessages.Model.StreamQueueMessagesRequest

data StreamQueueMessages (m :: Type -> Type) k where
  StreamQueueMessages :: StreamQueueMessagesRequest -> StreamQueueMessages m ()

makeSmartConstructors ''StreamQueueMessages
