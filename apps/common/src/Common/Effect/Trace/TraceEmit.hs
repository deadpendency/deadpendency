{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.Trace.TraceEmit
  ( startSpanComponentTrace,
    concludeSpan,
    startSpan,
    generateRunTrace,
    TraceEmit (..),
  )
where

import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Model.Details.RunTrace
import Control.Effect.TH

data TraceEmit (m :: Type -> Type) k where
  GenerateRunTrace :: TraceEmit m RunTrace
  StartSpan :: StartSpanRequest -> TraceEmit m StartSpanResult
  StartSpanComponentTrace :: Text -> StartSpanRequest -> TraceEmit m StartSpanResult
  ConcludeSpan :: ConcludeSpanRequest -> TraceEmit m ()

makeSmartConstructors ''TraceEmit
