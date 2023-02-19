module Common.Loader.GHAppAuthLoader (loadGitHubAppAuth) where

import Common.GitHub.Auth (generateSharedGHAppAuth, loadAuthPrereqs)
import Common.Model.GitHub.Auth.GHAppAuthGlobal
import Common.Model.GitHub.GHAppId
import Common.Model.GitHub.GHAppRawPrivateKey
import Control.Concurrent (newMVar)
import System.IO.Error (ioError, userError)

loadGitHubAppAuth :: GHAppId -> GHAppRawPrivateKey -> Bool -> IO GHAppAuthGlobal
loadGitHubAppAuth appId rawPrivateKey preloadJWT = do
  putTextLn "Loading GHAppAuthGlobal"
  let eitherAppAuthPrereqs = loadAuthPrereqs appId rawPrivateKey
  appAuthPreReqs <-
    case eitherAppAuthPrereqs of
      Right appAuthPreReqs' -> pure appAuthPreReqs'
      Left e -> ioError (userError $ "Unable to load app private key: " <> show @String e)

  maybeAppAuth <-
    if preloadJWT
      then do
        eitherAppAuth <- generateSharedGHAppAuth appAuthPreReqs
        case eitherAppAuth of
          Right appAuth -> pure $ Just appAuth
          Left e -> ioError (userError $ "Unable to build app auth jwt token: " <> show @String e)
      else pure Nothing
  mvarAppAuth <- newMVar maybeAppAuth
  let authGlobal =
        GHAppAuthGlobal
          { _appAuthPrereqs = appAuthPreReqs,
            _loadedAppAuth = mvarAppAuth
          }
  putTextLn "GHAppAuthGlobal Loaded"
  pure authGlobal
