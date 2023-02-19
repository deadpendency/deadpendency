module FD.TheMain
  ( theMain,
  )
where

import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import FD.AppGoogleScopes (AppGoogleScopes)
import FD.Loader.ComponentDetailsLoader (loadComponentDetails)
import FD.Loader.ConfigLoader (loadConfig)
import FD.Loader.GitHubKeyLoader (loadGitHubKey)
import FD.Model.AppContext (AppContext (..))
import FD.Serve.Server (runServer)

theMain :: IO ()
theMain = do
  commonConfig <- loadCommonConfig
  config <- loadConfig
  httpManager <- loadHttpManager
  instanceConfig <- loadInstanceConfig httpManager
  googleEnv <- loadGoogleEnv @AppGoogleScopes httpManager
  loadedGitHubKey <- loadGitHubKey googleEnv config
  let componentDetails = loadComponentDetails
      appContext =
        AppContext
          { _googleEnv = googleEnv,
            _config = config,
            _commonConfig = commonConfig,
            _instanceConfig = instanceConfig,
            _componentDetails = componentDetails,
            _loadedGitHubKey = loadedGitHubKey
          }

  runServer appContext
