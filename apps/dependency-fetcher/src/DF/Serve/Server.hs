module DF.Serve.Server
  ( runServer,
  )
where

import DF.Handler.DependencyFetcherHandler (dependencyFetcherHandler)
import DF.Model.AppContext
import DF.Serve.Api (AppApi (..))
import DF.Serve.AppHandler (CarrierStack, appToHandler)
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
    { dependencyFetcher = dependencyFetcherHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
