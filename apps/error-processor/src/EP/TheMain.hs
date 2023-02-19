module EP.TheMain
  ( theMain,
  )
where

import Common.Loader.CommonConfigLoader (loadCommonConfig)
import Common.Loader.GoogleEnvLoader (loadGoogleEnv)
import Common.Loader.HttpManagerLoader (loadHttpManager)
import Common.Loader.InstanceConfigLoader (loadInstanceConfig)
import EP.AppGoogleScopes (AppGoogleScopes)
import EP.Loader.ComponentDetailsLoader (loadComponentDetails)
import EP.Model.AppContext (AppContext (..))
import EP.Serve.Server (runServer)
import Network.Google.Env qualified as G

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
