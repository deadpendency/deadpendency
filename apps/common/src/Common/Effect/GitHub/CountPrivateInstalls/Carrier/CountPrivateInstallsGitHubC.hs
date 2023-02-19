module Common.Effect.GitHub.CountPrivateInstalls.Carrier.CountPrivateInstallsGitHubC
  ( CountPrivateInstallsGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.CountPrivateInstalls.Backend.CountPrivateInstallsBackend
import Common.Effect.GitHub.CountPrivateInstalls.CountPrivateInstalls (CountPrivateInstalls (..))
import Common.Effect.GitHub.CountPrivateInstalls.Model.CountPrivateInstallsResult
import Common.Effect.GitHub.InstallationAuth.InstallationAuth (InstallationAuth (..), existingInstallationAuth)
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)

newtype CountPrivateInstallsGitHubIOC m a = CountPrivateInstallsGitHubIOC {runCountPrivateInstallsGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has InstallationAuth sig m,
    Has (Throw CommonError) sig m
  ) =>
  Algebra (CountPrivateInstalls :+: sig) (CountPrivateInstallsGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L PrivateInstallsCount) -> do
      emitAppEventInfo (AppEventMessage "Started: Count Private Installs")
      ghAuth <- existingInstallationAuth
      eitherCount <- liftIO $ githubCountPrivateInstalls ghAuth
      count <- liftEither eitherCount
      let result =
            CountPrivateInstallsResult
              { _count = count
              }
      emitAppEventInfoA (AppEventMessage "Finished: Count Private Installs") (AppEventAdditional result)
      CountPrivateInstallsGitHubIOC $ pure (ctx $> result)
    (R other) -> CountPrivateInstallsGitHubIOC $ alg (runCountPrivateInstallsGitHubIOC . hdl) other ctx
