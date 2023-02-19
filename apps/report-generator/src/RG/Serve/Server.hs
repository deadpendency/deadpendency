module RG.Serve.Server
  ( runServer,
  )
where

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import RG.Handler.ReportGeneratorHandler (reportGeneratorHandler)
import RG.Model.AppContext
import RG.Serve.Api (AppApi (..))
import RG.Serve.AppHandler (CarrierStack, appToHandler)
import Servant.Server.Generic (AsServerT, genericServeT)

webAppGeneric :: AppContext -> Wai.Application
webAppGeneric appContext =
  genericServeT
    (appToHandler appContext)
    api

api :: AppApi (AsServerT CarrierStack)
api =
  AppApi
    { reportGenerator = reportGeneratorHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
