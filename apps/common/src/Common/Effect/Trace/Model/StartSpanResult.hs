module Common.Effect.Trace.Model.StartSpanResult
  ( StartSpanResult (..),
  )
where

import Common.Effect.Trace.Model.Span

newtype StartSpanResult = StartSpanResult
  { _span :: Span
  }
  deriving stock (Eq, Show, Generic)
