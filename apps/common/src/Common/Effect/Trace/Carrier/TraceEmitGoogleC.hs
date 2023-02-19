{-# LANGUAGE DataKinds #-}

module Common.Effect.Trace.Carrier.TraceEmitGoogleC
  ( TraceEmitGoogleIOC (..),
  )
where

import Common.Effect.Trace.Model.Span
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit (TraceEmit (..))
import Common.Effect.Util
import Common.Model.Details.ComponentTrace
import Common.Model.Details.RunTrace
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Random (Random, uniform)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.State (State, put)
import Control.Effect.Throw (Throw)
import Data.WideWord.Word128 (Word128 (..), showHexWord128)
import GHC.TypeLits (Symbol)
import Network.Google qualified as GB
import Network.Google.Auth.Scope qualified as G
import Network.Google.CloudTrace.Types qualified as G
import Network.Google.Env qualified as G
import Network.Google.Resource.CloudTrace.Projects.Traces.BatchWrite qualified as G
import Text.Printf (printf)

newtype TraceEmitGoogleIOC (s :: [Symbol]) m a = TraceEmitGoogleIOC {runTraceEmitGoogleIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has (Reader (G.Env s)) sig m,
    Has (State (Maybe RunTrace)) sig m,
    Has (State (Maybe ComponentTrace)) sig m,
    Has (Throw CommonError) sig m,
    Has Random sig m,
    G.AllowScopes s,
    G.HasScope' s TraceRequiredScopes ~ 'True,
    MonadIO m
  ) =>
  Algebra (TraceEmit :+: sig) (TraceEmitGoogleIOC s m)
  where
  alg hdl sig ctx = case sig of
    (L GenerateRunTrace) -> generateTraceId <&> RunTrace >>= TraceEmitGoogleIOC . pure . (ctx $>)
    (L (StartSpan startSpanRequest)) -> do
      span <- startSpan startSpanRequest
      let startSpanResult = StartSpanResult span
      TraceEmitGoogleIOC $ pure (ctx $> startSpanResult)
    (L (StartSpanComponentTrace traceId startSpanRequest)) -> do
      span <- startSpan startSpanRequest
      put $ Just $ ComponentTrace (span ^. #_spanId) traceId
      let startSpanResult = StartSpanResult span
      TraceEmitGoogleIOC $ pure (ctx $> startSpanResult)
    (L (ConcludeSpan concludeSpanRequest)) -> do
      currentTime <- liftIO getCurrentTime
      env <- ask @(G.Env s)
      runTrace <- getRunTrace
      let span = concludeSpanRequest ^. #_span
          traceId = runTrace ^. #_ntText

      let batchSpansRequest = buildGoogleSpanRequest traceId currentTime span
          projectsTracesBatchWrite = G.projectsTracesBatchWrite batchSpansRequest "projects/dgtw-deadpendency-action-2"
          innerLogic = (GB.runResourceT . GB.runGoogle env . GB.send) projectsTracesBatchWrite
      liftIO innerLogic $> ctx
    (R other) -> TraceEmitGoogleIOC $ alg (runTraceEmitGoogleIOC . hdl) other ctx

startSpan ::
  ( MonadIO m,
    Has Random sig m
  ) =>
  StartSpanRequest ->
  m Span
startSpan startSpanRequest = do
  currentTime <- liftIO getCurrentTime
  spanId <- generateSpanId
  let spanName = startSpanRequest ^. #_spanName
  pure
    Span
      { _spanId = spanId,
        _spanName = spanName,
        _startTime = currentTime
      }

type TraceRequiredScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform",
     "https://www.googleapis.com/auth/trace.append"
   ]

buildGoogleSpanRequest :: Text -> UTCTime -> Span -> G.BatchWriteSpansRequest
buildGoogleSpanRequest traceId currentTime (Span spanId spanName startTime) =
  let fullSpanName = "projects/dgtw-deadpendency-action-2/traces/" <> traceId <> "/spans/" <> spanId
      displayName =
        G.truncatableString
          & G.tsValue ?~ spanName
          & G.tsTruncatedByteCount ?~ 128
   in G.batchWriteSpansRequest
        & G.bwsrSpans
          .~ [ G.span
                 & G.sName ?~ fullSpanName
                 & G.sSpanId ?~ spanId
                 & G.sDisplayName ?~ displayName
                 & G.sStartTime ?~ startTime
                 & G.sEndTime ?~ currentTime
             ]

generateSpanId :: (Has Random sig m) => m Text
generateSpanId = do
  (word64 :: Word64) <- uniform
  pure . pack $ printf "%016x" word64

generateTraceId :: (Has Random sig m) => m Text
generateTraceId = do
  low <- uniform
  high <- uniform
  let word128 = Word128 low high
  pure . pack . printf "%032s" $ showHexWord128 word128
