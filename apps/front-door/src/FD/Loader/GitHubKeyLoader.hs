module FD.Loader.GitHubKeyLoader
  ( loadGitHubKey,
  )
where

import Common.Loader.SecretLoader
import FD.AppGoogleScopes
import FD.Model.Config
import FD.Model.LoadedGitHubKey
import Network.Google qualified as G
import Servant.GitHub.Webhook qualified as SGW

loadGitHubKey :: G.Env AppGoogleScopes -> Config -> IO LoadedGitHubKey
loadGitHubKey googleEnv config = do
  let keyName = config ^. #_githubWebhookSecretName
  -- this bind should ensure the key is loaded once, rather than many times
  secretData <- loadSecret googleEnv keyName
  pure $ LoadedGitHubKey (SGW.gitHubKey (pure secretData))
