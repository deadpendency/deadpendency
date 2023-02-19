module Common.Effect.Trace.Model.StartSpanRequest
  ( StartSpanRequest (..),
  )
where

newtype StartSpanRequest = StartSpanRequest
  { _spanName :: Text
  }
  deriving stock (Eq, Show, Generic)
