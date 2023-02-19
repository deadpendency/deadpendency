{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.QueueEventPublish.QueueEventPublish
  ( QueueEventPublish (..),
    publishQueueEvent,
  )
where

import Common.Effect.QueueEventPublish.Model.QueueEventPublishRequest
import Control.Effect.TH

data QueueEventPublish (m :: Type -> Type) k where
  PublishQueueEvent :: QueueEventPublishRequest -> QueueEventPublish m ()

makeSmartConstructors ''QueueEventPublish
