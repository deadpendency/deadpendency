module RG.TheMain
  ( theMain,
  )
where

import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import Network.Google.Env qualified as G
import RG.AppGoogleScopes (AppGoogleScopes)
import RG.Loader.ComponentDetailsLoader (loadComponentDetails)
import RG.Model.AppContext (AppContext (..))
import RG.Serve.Server (runServer)

theMain :: IO ()
theMain = do
  commonConfig <- loadCommonConfig
  httpManager <- loadHttpManager
  instanceConfig <- loadInstanceConfig httpManager
  (googleEnv :: G.Env AppGoogleScopes) <- loadGoogleEnv httpManager
  let componentDetails = loadComponentDetails
  let appContext =
        AppContext
          { _googleEnv = googleEnv,
            _commonConfig = commonConfig,
            _instanceConfig = instanceConfig,
            _componentDetails = componentDetails
          }

  runServer appContext
