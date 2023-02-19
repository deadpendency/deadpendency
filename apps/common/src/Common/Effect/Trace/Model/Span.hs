module Common.Effect.Trace.Model.Span
  ( Span (..),
  )
where

data Span = Span
  { _spanId :: Text,
    _spanName :: Text,
    _startTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
