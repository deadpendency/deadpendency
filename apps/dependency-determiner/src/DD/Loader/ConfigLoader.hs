module DD.Loader.ConfigLoader
  ( loadConfig,
  )
where

import Common.Model.GitHub.GHAppId
import DD.Model.Config (Config (..))
import System.Environment (getEnv)
import System.IO.Error (ioError, userError)

loadConfig :: IO Config
loadConfig = do
  putTextLn "Loading App Config"
  gitHubKey <- toText <$> getEnv "GITHUB_PRIVATE_KEY_SECRET_NAME"
  appId <- flip whenNothingM (ioError (userError "Error - APP_ID env variable not an integer")) $ getEnv "APP_ID" <&> readMaybe
  let config =
        Config
          { _githubPrivateKeySecretName = gitHubKey,
            _appId = GHAppId appId
          }
  putTextLn "App Config Loaded"
  pure config
