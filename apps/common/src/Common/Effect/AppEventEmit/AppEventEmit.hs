{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.AppEventEmit.AppEventEmit
  ( emitAppEvent,
    emitAppEventInfo,
    emitAppEventInfoA,
    emitAppEventErrorA,
    AppEventEmit (..),
  )
where

import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Control.Algebra (Has, send)
import Control.Effect.TH

data AppEventEmit (m :: Type -> Type) k where
  EmitAppEvent :: AppEvent -> AppEventEmit m ()

makeSmartConstructors ''AppEventEmit

emitAppEventInfo :: (Has AppEventEmit sig m) => AppEventMessage -> m ()
emitAppEventInfo message = send $ EmitAppEvent appEvent
  where
    appEvent = AppEvent AppEventLevelInfo message Nothing

emitAppEventInfoA :: (Has AppEventEmit sig m) => AppEventMessage -> AppEventAdditional -> m ()
emitAppEventInfoA message additional = send $ EmitAppEvent appEvent
  where
    appEvent = AppEvent AppEventLevelInfo message (Just additional)

emitAppEventErrorA :: (Has AppEventEmit sig m) => AppEventMessage -> AppEventAdditional -> m ()
emitAppEventErrorA message additional = send $ EmitAppEvent appEvent
  where
    appEvent = AppEvent AppEventLevelError message (Just additional)
