module RP.Serve.Server
  ( runServer,
  )
where

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import RP.Handler.RunPreparerHandler (runPreparerHandler)
import RP.Model.AppContext
import RP.Serve.Api (AppApi (..))
import RP.Serve.AppHandler
import Servant.Server.Generic (AsServerT, genericServeT)

webAppGeneric :: AppContext -> Wai.Application
webAppGeneric appContext =
  genericServeT
    (appToHandler appContext)
    api

api :: AppApi (AsServerT CarrierStack)
api =
  AppApi
    { prepareRun = runPreparerHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
