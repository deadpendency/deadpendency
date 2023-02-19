module SR.Serve.Server
  ( runServer,
  )
where

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import SR.Handler.EmitTotalInstallsMetricHandler
import SR.Handler.ReplayFailedHandler (replayFailedHandler)
import SR.Handler.RunSmokeTestHandler
import SR.Model.AppContext
import SR.Serve.Api (AppApi (..))
import SR.Serve.AppHandler (CarrierStack, appToHandler)
import Servant.Server.Generic (AsServerT, genericServeT)

webAppGeneric :: AppContext -> Wai.Application
webAppGeneric appContext =
  genericServeT
    (appToHandler appContext)
    api

api :: AppApi (AsServerT CarrierStack)
api =
  AppApi
    { replayFailed = replayFailedHandler,
      runSmokeTest = runSmokeTestHandler,
      emitTotalInstalls = emitTotalInstallsMetricHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
