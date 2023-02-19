module Common.Effect.GitHub.InstallationAuth.Carrier.InstallationAuthGitHubC
  ( InstallationAuthGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.AppSharedAuth.AppSharedAuth (AppSharedAuth (..), obtainAppSharedAuth)
import Common.Effect.GitHub.InstallationAuth.InstallationAuth (InstallationAuth (..))
import Common.Effect.Util
import Common.GitHub.Auth (generateInstallationAuth)
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.State (State, put)
import Control.Effect.Throw (Throw)

newtype InstallationAuthGitHubIOC m a = InstallationAuthGitHubIOC {runInstallationAuthGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has (State (Maybe GHInstallationAuth)) sig m,
    Has (Throw CommonError) sig m,
    Has AppSharedAuth sig m
  ) =>
  Algebra (InstallationAuth :+: sig) (InstallationAuthGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (ObtainInstallationAuth ghInstallationId)) -> do
      emitAppEventInfo (AppEventMessage "Started: Obtain Installation Auth")
      sharedAuth <- obtainAppSharedAuth
      installAuth <- liftIO $ generateInstallationAuth ghInstallationId sharedAuth
      put $ Just installAuth
      InstallationAuthGitHubIOC $ pure (ctx $> installAuth)
    (L ExistingInstallationAuth) -> do
      emitAppEventInfo (AppEventMessage "Started: Existing Installation Auth")
      ghInstallationAuth <- getInstallAuth
      let ghInstallationAuthExpiryTime = ghInstallationAuth ^. #_expirationTime
          ghInstallationId = ghInstallationAuth ^. #_installationId
      currentTime <- liftIO getCurrentTime
      let diffTime = diffUTCTime ghInstallationAuthExpiryTime currentTime
      -- we leave 20 seconds buffer to allow remaining operations to complete
      refreshedAuth <-
        if diffTime < 20
          then do
            emitAppEventInfo
              (AppEventMessage $ "Started: Generate New Installation Auth - Old Expires in: " <> show @Text diffTime)
            sharedAuth <- obtainAppSharedAuth
            newInstallAuth <- liftIO $ generateInstallationAuth ghInstallationId sharedAuth
            emitAppEventInfoA (AppEventMessage "Finished: Generate New Installation Auth") (AppEventAdditional newInstallAuth)
            pure newInstallAuth
          else pure ghInstallationAuth
      put $ Just refreshedAuth
      emitAppEventInfoA (AppEventMessage "Finished: Existing Installation Auth") (AppEventAdditional refreshedAuth)
      InstallationAuthGitHubIOC $ pure (ctx $> refreshedAuth)
    (R other) -> InstallationAuthGitHubIOC $ alg (runInstallationAuthGitHubIOC . hdl) other ctx
