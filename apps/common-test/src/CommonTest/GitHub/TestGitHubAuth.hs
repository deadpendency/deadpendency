{-# LANGUAGE DataKinds #-}

-- |
-- Description : For authing with github when running integration tests.
module CommonTest.GitHub.TestGitHubAuth (reloadOrGenerateAuth) where

import Common.GitHub.Auth
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.SecretLoader
import Common.Model.Config.AppEnv
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppId
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHAppRawPrivateKey
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Network.Google qualified as G

authCacheFile :: AppEnv -> FilePath
authCacheFile appEnv = "../temp/temp-gh-installation-auth-" <> unpack (appEnvAsText appEnv) <> ".json"

reloadOrGenerateAuth :: AppEnv -> IO GHInstallationAuth
reloadOrGenerateAuth appEnv = do
  putTextLn "Loading GH Install Auth"
  maybeAuth <- readAuthFromFile appEnv
  case maybeAuth of
    Just auth -> do
      valid <- isAuthStillValid auth
      if valid
        then putTextLn "Cached auth is valid reusing.." $> auth
        else putTextLn "No or old cached auth, regenerating.." *> produceNewAppAuth appEnv
    Nothing ->
      putTextLn "No cached auth found, regenerating.." *> produceNewAppAuth appEnv

persistAuthToFile :: AppEnv -> GHInstallationAuth -> IO ()
persistAuthToFile appEnv installAuth = do
  eitherWriteResult <- try @_ @IOException $ encodeFile (authCacheFile appEnv) installAuth
  case eitherWriteResult of
    Right _ -> pure ()
    Left e -> putTextLn ("Exception while writing file: " <> show @Text e) *> persistAuthToFile appEnv installAuth

readAuthFromFile :: AppEnv -> IO (Maybe GHInstallationAuth)
readAuthFromFile appEnv = do
  eitherMaybeAuth <- try @_ @IOException $ eitherDecodeFileStrict' (authCacheFile appEnv)
  case eitherMaybeAuth of
    Right eitherInstallAuth -> case eitherInstallAuth of
      Right installAuth -> putTextLn "Loaded auth from file" $> Just installAuth
      Left errorString -> error ("Failure to decode auth: " <> show @Text errorString)
    Left e -> putTextLn ("Exception while loading file: " <> show @Text e) $> Nothing

isAuthStillValid :: GHInstallationAuth -> IO Bool
isAuthStillValid ghInstallationAuth = do
  let ghInstallationAuthExpiryTime = ghInstallationAuth ^. #_expirationTime
  currentTime <- getCurrentTime
  let diffTime = diffUTCTime ghInstallationAuthExpiryTime currentTime
  -- we leave 20 seconds buffer to allow remaining operations to complete
  putTextLn $ "Auth expiring in: " <> show @Text diffTime
  pure (diffTime > 20)

produceNewAppAuth :: AppEnv -> IO GHInstallationAuth
produceNewAppAuth appEnv = do
  googleEnv <- initGoogleEnv
  let secretName = appEnvAsKeyPrefix appEnv <> "-github-app-private-key-secret"
      appId =
        GHAppId $
          case appEnv of
            Prod -> 51614
            PreProd -> 77327
            Test -> 77327

  ghAppPrivateKey <- GHAppRawPrivateKey <$> loadSecret googleEnv secretName
  let eitherAppAuthPrereqs = loadAuthPrereqs appId ghAppPrivateKey
  appAuthPreReqs <-
    case eitherAppAuthPrereqs of
      Right appAuthPreReqs' -> pure appAuthPreReqs'
      Left _ -> error "Unable to load app private key"

  eitherAppSharedAuth <- generateSharedGHAppAuth appAuthPreReqs
  let appSharedAuth =
        case eitherAppSharedAuth of
          Right auth -> auth
          Left _ -> error "failure to auth"
  let appInstallationId =
        GHAppInstallationId $
          case appEnv of
            Prod -> 11238978
            PreProd -> 11324639
            Test -> 11324639

  putTextLn "Requesting install auth"
  newAuth <- generateInstallationAuth appInstallationId appSharedAuth
  persistAuthToFile appEnv newAuth
  pure newAuth

appEnvAsKeyPrefix :: AppEnv -> Text
appEnvAsKeyPrefix =
  \case
    Prod -> "prod"
    PreProd -> "preprod"
    Test -> "preprod"

initGoogleEnv :: IO (G.Env TestGoogleScopes)
initGoogleEnv =
  loadHttpManager >>= loadGoogleEnv @TestGoogleScopes

type TestGoogleScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform"
   ]
