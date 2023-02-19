module RF.TheMain
  ( theMain,
  )
where

import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GHAppAuthLoader (loadGitHubAppAuth)
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import Common.Loader.SecretLoader
import Common.Model.GitHub.GHAppRawPrivateKey
import RF.AppGoogleScopes (AppGoogleScopes)
import RF.Loader.ComponentDetailsLoader (loadComponentDetails)
import RF.Loader.ConfigLoader
import RF.Model.AppContext (AppContext (..))
import RF.Serve.Server (runServer)

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
  ghAppAuthGlobal <- loadGitHubAppAuth appId ghAppPrivateKey False
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
