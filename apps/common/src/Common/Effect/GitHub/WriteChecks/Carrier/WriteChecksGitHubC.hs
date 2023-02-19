module Common.Effect.GitHub.WriteChecks.Carrier.WriteChecksGitHubC
  ( WriteChecksGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.InstallationAuth.InstallationAuth
import Common.Effect.GitHub.WriteChecks.Backend.CreateCheckRunBackend
import Common.Effect.GitHub.WriteChecks.Backend.UpdateCheckRunBackend
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateResult
import Common.Effect.GitHub.WriteChecks.WriteChecks (WriteChecks (..))
import Common.Model.Error.CommonError
import Common.Model.Error.WriteChecksError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)

newtype WriteChecksGitHubIOC m a = WriteChecksGitHubIOC {runWriteChecksGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has (Throw CommonError) sig m,
    Has (Throw WriteChecksError) sig m,
    Has InstallationAuth sig m
  ) =>
  Algebra (WriteChecks :+: sig) (WriteChecksGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (CreateCheckRun request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Create check run") (AppEventAdditional request)
      let installId = request ^. #_appInstallationId
      ghAuth <- obtainInstallationAuth installId
      eitherCreateCheckRunResult <- liftIO $ githubCreateCheckRun ghAuth request
      liftedErrorResult <- liftEither eitherCreateCheckRunResult
      emitAppEventInfoA (AppEventMessage "Finished: Create check run") (AppEventAdditional liftedErrorResult)
      WriteChecksGitHubIOC $ pure (ctx $> liftedErrorResult)
    (L (UpdateCheckRun request)) -> do
      -- currently the checkRunOutput can be huge, so can't always log it
      emitAppEventInfoA (AppEventMessage "Started: Update check run") (AppEventAdditional $ request & #_checkRunOutput .~ Nothing)
      ghAuth <- existingInstallationAuth
      eitherUpdateCheckRunResult <- liftIO $ githubUpdateCheckRun ghAuth request
      liftedCheckRun <- liftEither eitherUpdateCheckRunResult
      let result = CheckRunUpdateResult liftedCheckRun
      emitAppEventInfoA (AppEventMessage "Finished: Update check run") (AppEventAdditional result)
      WriteChecksGitHubIOC $ pure (ctx $> result)
    (R other) -> WriteChecksGitHubIOC $ alg (runWriteChecksGitHubIOC . hdl) other ctx
