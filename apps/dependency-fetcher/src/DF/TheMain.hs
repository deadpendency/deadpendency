module DF.TheMain
  ( theMain,
  )
where

import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GHAppAuthLoader (loadGitHubAppAuth)
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import Common.Loader.RedisConnectionLoader (loadRedisConnectionGlobal)
import Common.Loader.SecretLoader
import Common.Model.GitHub.GHAppRawPrivateKey
import DF.AppGoogleScopes (AppGoogleScopes)
import DF.Loader.ComponentDetailsLoader (loadComponentDetails)
import DF.Loader.ConfigLoader (loadConfig)
import DF.Model.AppContext (AppContext (..))
import DF.Serve.Server (runServer)

theMain :: IO ()
theMain = do
  commonConfig <- loadCommonConfig
  appConfig <- loadConfig
  httpManager <- loadHttpManager
  instanceConfig <- loadInstanceConfig httpManager
  googleEnv <- loadGoogleEnv @AppGoogleScopes httpManager
  let webhookPrivateKeyName = appConfig ^. #_githubPrivateKeySecretName
      appId = appConfig ^. #_appId
  ghAppPrivateKey <- GHAppRawPrivateKey <$> loadSecret googleEnv webhookPrivateKeyName
  ghAppAuthGlobal <- loadGitHubAppAuth appId ghAppPrivateKey False
  cacheConfigGlobal <- loadRedisConnectionGlobal (appConfig ^. #_redisDatabaseHost)
  let componentDetails = loadComponentDetails
  let appContext =
        AppContext
          { _googleEnv = googleEnv,
            _commonConfig = commonConfig,
            _instanceConfig = instanceConfig,
            _componentDetails = componentDetails,
            _ghAppAuthGlobal = ghAppAuthGlobal,
            _cacheConfigGlobal = cacheConfigGlobal
          }

  runServer appContext
