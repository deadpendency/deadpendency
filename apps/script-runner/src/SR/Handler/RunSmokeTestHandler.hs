{-# LANGUAGE DataKinds #-}

module SR.Handler.RunSmokeTestHandler
  ( runSmokeTestHandler,
    RunSmokeTestRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.MetricEmit.MetricEmit
import Common.Effect.MetricEmit.Model.MetricEvent
import Control.Algebra (Has)
import Data.Vector qualified as V
import SR.Effect.RunSmokeTest.Model.SmokeResult
import SR.Effect.RunSmokeTest.RunSmokeTest
import Servant (JSON, Post, (:>))
import Servant.API (QueryFlag)

type RunSmokeTestRoute =
  "run-smoke-test"
    :> QueryFlag "emit-metric"
    :> Post '[JSON] Text

runSmokeTestHandler ::
  ( Has AppEventEmit sig m,
    Has RunSmokeTest sig m,
    Has MetricEmit sig m
  ) =>
  Bool ->
  m Text
runSmokeTestHandler shouldEmitMetric = do
  emitAppEventInfo (AppEventMessage "Received Run Smoke Test")

  smokeResults <- runSmokeTest

  let failures = V.mapMaybe toFailureText smokeResults
      smokeSuccessful = V.null failures
      textualResult =
        if smokeSuccessful
          then "Success"
          else fold $ intersperseV "\n" failures

  when
    shouldEmitMetric
    (metricEmit (SmokeTestResultEvent smokeSuccessful))

  emitAppEventInfoA (AppEventMessage "Finished Run Smoke Test") (AppEventAdditional textualResult)
  pure textualResult

toFailureText :: SmokeResult -> Maybe Text
toFailureText =
  \case
    SRSuccess -> Nothing
    SRException repo failure -> Just $ "Exception for " <> repo ^. (#_repoName . #_ntText) <> "\n" <> failure
    SRFailure repo theseMismatch -> Just $ "Exception for " <> repo ^. (#_repoName . #_ntText) <> "\n" <> toTheseFailures theseMismatch

toTheseFailures :: These DepsMismatch ErrorsMismatch -> Text
toTheseFailures =
  \case
    This depsMismatch -> "Deps Mismatch\n" <> depsMismatch ^. #_ntText <> "\n"
    That errorsMismatch -> "Errors Mismatch\n" <> errorsMismatch ^. #_ntText <> "\n"
    These depsMismatch errorsMismatch ->
      "Deps Mismatch\n"
        <> depsMismatch
          ^. #_ntText
        <> "\n"
        <> "Errors Mismatch\n"
        <> errorsMismatch
          ^. #_ntText
        <> "\n"
