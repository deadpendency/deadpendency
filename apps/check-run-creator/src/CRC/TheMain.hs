module CRC.TheMain
  ( theMain,
  )
where

import CRC.AppGoogleScopes (AppGoogleScopes)
import CRC.Loader.ComponentDetailsLoader (loadComponentDetails)
import CRC.Loader.ConfigLoader
import CRC.Model.AppContext (AppContext (..))
import CRC.Serve.Server (runServer)
import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GHAppAuthLoader (loadGitHubAppAuth)
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import Common.Loader.SecretLoader
import Common.Model.GitHub.GHAppRawPrivateKey

theMain :: IO ()
theMain = do
  commonConfig <- loadCommonConfig
  config <- loadConfig
  httpManager <- loadHttpManager
  instanceConfig <- loadInstanceConfig httpManager
  googleEnv <- loadGoogleEnv @AppGoogleScopes httpManager
  let webhookPrivateKeyName = config ^. #_githubPrivateKeySecretName
      appId = config ^. #_appId
  ghAppPrivateKey <- GHAppRawPrivateKey <$> loadSecret googleEnv webhookPrivateKeyName
  ghAppAuthGlobal <- loadGitHubAppAuth appId ghAppPrivateKey True -- preload as crc will always do the initial install auth
  let componentDetails = loadComponentDetails
  let appContext =
        AppContext
          { _googleEnv = googleEnv,
            _commonConfig = commonConfig,
            _instanceConfig = instanceConfig,
            _componentDetails = componentDetails,
            _ghAppAuthGlobal = ghAppAuthGlobal
          }

  runServer appContext
