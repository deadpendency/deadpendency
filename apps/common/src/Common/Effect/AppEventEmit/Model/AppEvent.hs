module Common.Effect.AppEventEmit.Model.AppEvent
  ( AppEvent (..),
  )
where

import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel (AppEventLevel)
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage)

data AppEvent = AppEvent
  { _level :: AppEventLevel,
    _message :: AppEventMessage,
    _additional :: Maybe AppEventAdditional
  }
  deriving stock (Eq, Show, Generic)
