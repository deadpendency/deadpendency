{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.PublishFailedMessage.PublishFailedMessage
  ( PublishFailedMessage (..),
    publishFailedMessage,
  )
where

import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Control.Effect.TH

data PublishFailedMessage p (m :: Type -> Type) k where
  PublishFailedMessage :: FailedInterchangeEvent p -> PublishFailedMessage p m ()

makeSmartConstructors ''PublishFailedMessage
