module SR.TheMain
  ( theMain,
  )
where

import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GHAppAuthLoader
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import Common.Loader.SecretLoader
import Common.Model.GitHub.GHAppRawPrivateKey
import Network.Google.Env qualified as G
import SR.AppGoogleScopes (AppGoogleScopes)
import SR.Loader.ComponentDetailsLoader (loadComponentDetails)
import SR.Loader.ConfigLoader
import SR.Model.AppContext (AppContext (..))
import SR.Serve.Server (runServer)

theMain :: IO ()
theMain = do
  commonConfig <- loadCommonConfig
  appConfig <- loadConfig
  httpManager <- loadHttpManager
  instanceConfig <- loadInstanceConfig httpManager
  (googleEnv :: G.Env AppGoogleScopes) <- loadGoogleEnv httpManager
  let webhookPrivateKeyName = appConfig ^. #_githubPrivateKeySecretName
      appId = appConfig ^. #_appId
  ghAppPrivateKey <- GHAppRawPrivateKey <$> loadSecret googleEnv webhookPrivateKeyName
  ghAppAuthGlobal <- loadGitHubAppAuth appId ghAppPrivateKey True
  let componentDetails = loadComponentDetails
  let appContext =
        AppContext
          { _googleEnv = googleEnv,
            _commonConfig = commonConfig,
            _instanceConfig = instanceConfig,
            _componentDetails = componentDetails,
            _appConfig = appConfig,
            _ghAppAuthGlobal = ghAppAuthGlobal
          }

  runServer appContext
