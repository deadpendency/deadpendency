module RF.Serve.Server
  ( runServer,
  )
where

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import RF.Handler.RunFinalizerHandler (runFinalizerHandler)
import RF.Model.AppContext
import RF.Serve.Api (AppApi (..))
import RF.Serve.AppHandler (CarrierStack, appToHandler)
import Servant.Server.Generic (AsServerT, genericServeT)

webAppGeneric :: AppContext -> Wai.Application
webAppGeneric appContext =
  genericServeT
    (appToHandler appContext)
    api

api :: AppApi (AsServerT CarrierStack)
api =
  AppApi
    { runFinalizer = runFinalizerHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
