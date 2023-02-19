module Common.Effect.Trace.Carrier.TraceEmitRetC
  ( runTraceEmitRet,
  )
where

import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit (TraceEmit (..))
import Common.Model.Details.RunTrace
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype TraceEmitRetC m a = TraceEmitRetC {runTraceEmitRetC :: ReaderC (RunTrace, StartSpanResult) (WriterC [StartSpanRequest] (WriterC [ConcludeSpanRequest] m)) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (TraceEmit :+: sig) (TraceEmitRetC m) where
  alg hdl sig ctx = case sig of
    (L GenerateRunTrace) -> do
      (runTrace, _) <- TraceEmitRetC $ ask @(RunTrace, StartSpanResult)
      TraceEmitRetC $ pure (ctx $> runTrace)
    (L (StartSpan startSpanRequest)) -> do
      TraceEmitRetC $ tell [startSpanRequest]
      (_, startSpanResult) <- TraceEmitRetC $ ask @(RunTrace, StartSpanResult)
      TraceEmitRetC $ pure (ctx $> startSpanResult)
    (L (StartSpanComponentTrace _ startSpanRequest)) -> do
      TraceEmitRetC $ tell [startSpanRequest]
      (_, startSpanResult) <- TraceEmitRetC $ ask @(RunTrace, StartSpanResult)
      TraceEmitRetC $ pure (ctx $> startSpanResult)
    (L (ConcludeSpan concludeSpanRequest)) -> do
      TraceEmitRetC $ tell [concludeSpanRequest] $> ctx
    (R other) -> TraceEmitRetC $ alg (runTraceEmitRetC . hdl) (R (R (R other))) ctx

runTraceEmitRet :: RunTrace -> StartSpanResult -> TraceEmitRetC m a -> m ([ConcludeSpanRequest], ([StartSpanRequest], a))
runTraceEmitRet runTrace startSpanResult = runWriter . runWriter . runReader (runTrace, startSpanResult) . runTraceEmitRetC
