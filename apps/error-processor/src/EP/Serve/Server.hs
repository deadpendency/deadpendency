module EP.Serve.Server
  ( runServer,
  )
where

import EP.Handler.PublishFailedReportHandler (publishFailedReportHandler)
import EP.Model.AppContext
import EP.Serve.Api (AppApi (..))
import EP.Serve.AppHandler (CarrierStack, appToHandler)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server.Generic (AsServerT, genericServeT)

webAppGeneric :: AppContext -> Wai.Application
webAppGeneric appContext =
  genericServeT
    (appToHandler appContext)
    api

api :: AppApi (AsServerT CarrierStack)
api =
  AppApi
    { publishFailedReport = publishFailedReportHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
