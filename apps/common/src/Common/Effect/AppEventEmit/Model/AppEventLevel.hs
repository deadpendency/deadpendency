module Common.Effect.AppEventEmit.Model.AppEventLevel (AppEventLevel (..)) where

data AppEventLevel
  = AppEventLevelError
  | AppEventLevelWarning
  | AppEventLevelInfo
  | AppEventLevelDebug
  deriving stock (Eq, Show)
