module FD.Loader.ConfigLoader
  ( loadConfig,
  )
where

import FD.Model.Config (Config (..))
import System.Environment (getEnv)

loadConfig :: IO Config
loadConfig = do
  putTextLn "Loading App Config"
  gitHubKey <- toText <$> getEnv "GITHUB_WEBHOOK_SECRET_NAME"
  let config =
        Config
          { _githubWebhookSecretName = gitHubKey
          }
  putTextLn "App Config Loaded"
  pure config
