module Common.Effect.Trace.Model.ConcludeSpanRequest
  ( ConcludeSpanRequest (..),
  )
where

import Common.Effect.Trace.Model.Span

newtype ConcludeSpanRequest = ConcludeSpanRequest
  { _span :: Span
  }
  deriving stock (Eq, Show, Generic)
