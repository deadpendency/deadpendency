module Common.Effect.GitHub.AppSharedAuth.Carrier.AppSharedAuthGenerateC
  ( AppSharedAuthGenerateC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.AppSharedAuth.AppSharedAuth (AppSharedAuth (..))
import Common.Effect.Util
import Common.GitHub.Auth
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHAppAuthGlobal
import Common.Model.GitHub.Auth.GHAppAuthPrereqs
import Common.Model.GitHub.Auth.GHSharedAppAuth
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Concurrent (putMVar, takeMVar)
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Throw (Throw, liftEither)

newtype AppSharedAuthGenerateC m a = AppSharedAuthGenerateC {runAppSharedAuthGenerateC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has (Throw CommonError) sig m,
    Has (Reader GHAppAuthGlobal) sig m
  ) =>
  Algebra (AppSharedAuth :+: sig) (AppSharedAuthGenerateC m)
  where
  alg hdl sig ctx = case sig of
    (L ObtainAppSharedAuth) -> do
      emitAppEventInfo (AppEventMessage "Started: Obtain App Auth Global")
      globalAppAuth <- ask @GHAppAuthGlobal
      let mvarMaybeAppSharedAuth = globalAppAuth ^. #_loadedAppAuth
          globalAppPrereqs = globalAppAuth ^. #_appAuthPrereqs

      eitherSharedAppAuth <- liftIO $ do
        maybeAppSharedAuth <- takeMVar mvarMaybeAppSharedAuth
        eitherUpdatedAppSharedAuth <-
          case maybeAppSharedAuth of
            Just sharedAppAuth -> reAuthIfExpiring globalAppPrereqs sharedAppAuth
            Nothing -> generateSharedGHAppAuth globalAppPrereqs
        case eitherUpdatedAppSharedAuth of
          Right updatedAppSharedAuth -> do
            putMVar mvarMaybeAppSharedAuth (Just updatedAppSharedAuth)
            pure $ Right updatedAppSharedAuth
          Left e -> do
            putMVar mvarMaybeAppSharedAuth maybeAppSharedAuth
            pure $ Left e

      let mappedLeft = first GitHubAppAuthCreationError eitherSharedAppAuth

      emitErrorIfLeft (AppEventMessage "Failed: Obtain App Auth Global") (pure mappedLeft)

      sharedAppAuth <- liftEither mappedLeft
      emitAppEventInfoA (AppEventMessage "Finished: Obtain App Auth Global") (AppEventAdditional sharedAppAuth)

      AppSharedAuthGenerateC $ pure (ctx $> sharedAppAuth)
    (R other) -> AppSharedAuthGenerateC $ alg (runAppSharedAuthGenerateC . hdl) other ctx

reAuthIfExpiring :: GHAppAuthPrereqs -> GHSharedAppAuth -> IO (Either Text GHSharedAppAuth)
reAuthIfExpiring prereqs sharedAppAuth = do
  let authExpiry = sharedAppAuth ^. #_expiryTime
  currentTime <- getCurrentTime
  let diffTime = diffUTCTime authExpiry currentTime
  -- we leave 20 seconds buffer to allow the thread to make use this token before it expires
  if diffTime < 20
    then generateSharedGHAppAuth prereqs
    else pure $ Right sharedAppAuth
