module DD.Serve.Server
  ( runServer,
  )
where

import DD.Handler.DependencyDeterminerHandler (dependencyDeterminerHandler)
import DD.Model.AppContext
import DD.Serve.Api (AppApi (..))
import DD.Serve.AppHandler (CarrierStack, appToHandler)
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
    { dependencyDeterminer = dependencyDeterminerHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
