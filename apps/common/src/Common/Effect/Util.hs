module Common.Effect.Util
  ( emitErrorIfLeft,
    getInstallAuth,
    maybeToErrorM,
    getRepoConfig,
    getRun,
    getCheckRun,
    getRunTrace,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Has)
import Control.Effect.State (State, get)
import Control.Effect.Throw (Throw, throwError)
import Data.Aeson (ToJSON)

getInstallAuth :: (Has (State (Maybe GHInstallationAuth)) sig m, Has (Throw CommonError) sig m) => m GHInstallationAuth
getInstallAuth = getStateLiftError @GHInstallationAuth GetInstallAuthBeforePut

getRepoConfig :: (Has (State (Maybe RepoConfig)) sig m, Has (Throw CommonError) sig m) => m RepoConfig
getRepoConfig = getStateLiftError @RepoConfig GetRepoConfigBeforePut

getRun :: (Has (State (Maybe Run)) sig m, Has (Throw CommonError) sig m) => m Run
getRun = getStateLiftError @Run GetRunBeforePut

getRunTrace :: (Has (State (Maybe RunTrace)) sig m, Has (Throw CommonError) sig m) => m RunTrace
getRunTrace = getStateLiftError @RunTrace GetRunTraceBeforePut

getCheckRun :: (Has (State (Maybe CheckRun)) sig m, Has (Throw CommonError) sig m) => m CheckRun
getCheckRun = getStateLiftError @CheckRun GetCheckRunBeforePut

getStateLiftError :: forall a e sig m. (Has (State (Maybe a)) sig m, Has (Throw e) sig m) => e -> m a
getStateLiftError e = maybeToErrorM e =<< (get @(Maybe a))

maybeToErrorM :: (Has (Throw e) sig m) => e -> Maybe a -> m a
maybeToErrorM _ (Just a) = pure a
maybeToErrorM e Nothing = throwError e

emitErrorIfLeft :: (Has (Throw l) sig m, Has AppEventEmit sig m, ToJSON l) => AppEventMessage -> m (Either l r) -> m ()
emitErrorIfLeft appEventMessage eitherM = do
  either' <- eitherM
  case either' of
    Left e -> emitAppEvent (AppEvent AppEventLevelError appEventMessage (Just $ AppEventAdditional e))
    Right _ -> pure ()
