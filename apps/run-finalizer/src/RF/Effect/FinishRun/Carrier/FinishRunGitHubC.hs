module RF.Effect.FinishRun.Carrier.FinishRunGitHubC
  ( FinishRunGithubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequestStatus
import Common.Effect.GitHub.WriteChecks.WriteChecks
import Common.Effect.Util
import Common.Model.Details.Run
import Common.Model.Error.CommonError
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.GitHub.Checks.Output.CheckRunOutput
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.State (State)
import Control.Effect.Throw (Throw)
import RF.Effect.FinishRun.Backend.GenerateFinishCheckRunBackend
import RF.Effect.FinishRun.FinishRun
import RF.Effect.FinishRun.Model.FinishRunResult

newtype FinishRunGithubIOC m a = FinishRunGithubIOC {runFinishRunGithubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (State (Maybe Run)) sig m,
    Has (State (Maybe CheckRun)) sig m,
    Has (Throw CommonError) sig m,
    Has WriteChecks sig m,
    MonadIO m
  ) =>
  Algebra (FinishRun :+: sig) (FinishRunGithubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (FinishRun request)) -> do
      emitAppEventInfo (AppEventMessage "Started: Finish run")
      run <- getRun
      checkRun <- getCheckRun
      let runResult = request ^. #_runResult
          finishCheckRun = generateFinishCheckRun runResult
          repoNodeId = run ^. #_repoNodeId
          checkRunNodeId = checkRun ^. #_nodeId
          updateRequestStatus = CheckRunUpdateRequestStatusCompleted
          conclusion = finishCheckRun ^. #_fcrCheckRunConclusion
          output =
            CheckRunOutput
              { _checkRunOutputTitle = finishCheckRun ^. #_fcrTitle,
                _checkRunOutputBody = finishCheckRun ^. #_fcrBody,
                _checkRunOutputSummary = finishCheckRun ^. #_fcrSummary
              }

          checkRunUpdateRequest =
            CheckRunUpdateRequest
              { _repoNodeId = repoNodeId,
                _checkRunNodeId = checkRunNodeId,
                _checkRunStatus = Just updateRequestStatus,
                _checkRunConclusion = Just conclusion,
                _checkRunOutput = Just output
              }

      checkRunUpdateResult <- updateCheckRun checkRunUpdateRequest
      let result = FinishRunResult $ checkRunUpdateResult ^. #_checkRun

      emitAppEventInfoA (AppEventMessage "Finished: Finish run") (AppEventAdditional result)
      FinishRunGithubIOC $ pure (ctx $> result)
    (R other) -> FinishRunGithubIOC $ alg (runFinishRunGithubIOC . hdl) other ctx
