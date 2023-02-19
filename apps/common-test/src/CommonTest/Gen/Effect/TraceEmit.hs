{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Effect.TraceEmit where

import Common.Effect.Trace.Model.Span
import Common.Effect.Trace.Model.StartSpanResult
import CommonTest.Gen.General
import Hedgehog

genStartSpanResult :: Gen StartSpanResult
genStartSpanResult = StartSpanResult <$> genSpan

genSpan :: Gen Span
genSpan = do
  spanId <- genAlphaText
  spanName <- genAlphaText
  startTime <- genUTCTime
  pure
    Span
      { _spanId = spanId,
        _spanName = spanName,
        _startTime = startTime
      }
