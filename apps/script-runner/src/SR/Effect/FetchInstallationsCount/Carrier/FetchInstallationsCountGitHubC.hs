module SR.Effect.FetchInstallationsCount.Carrier.FetchInstallationsCountGitHubC
  ( FetchInstallationsCountGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.AppSharedAuth.AppSharedAuth
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)
import SR.Effect.FetchInstallationsCount.Backend.FetchInstallationsCountBackend
import SR.Effect.FetchInstallationsCount.FetchInstallationsCount

newtype FetchInstallationsCountGitHubIOC m a = FetchInstallationsCountGitHubIOC {runFetchInstallationsCountGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has AppSharedAuth sig m,
    Has (Throw CommonError) sig m
  ) =>
  Algebra (FetchInstallationsCount :+: sig) (FetchInstallationsCountGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (InstallationsCountFetch plan)) -> do
      emitAppEventInfo (AppEventMessage "Started: Fetch Installations Count")
      ghAuth <- obtainAppSharedAuth

      eitherCount <- liftIO $ githubFetchInstallationsCount ghAuth plan
      count <- liftEither eitherCount
      emitAppEventInfoA (AppEventMessage "Finished: Fetch Installations Count") (AppEventAdditional count)

      FetchInstallationsCountGitHubIOC $ pure (ctx $> count)
    (R other) -> FetchInstallationsCountGitHubIOC $ alg (runFetchInstallationsCountGitHubIOC . hdl) other ctx
