{-# LANGUAGE PartialTypeSignatures #-}

module CRC.Serve.Server
  ( runServer,
  )
where

import CRC.Handler.CheckRunCreatorHandler (checkRunCreatorHandler)
import CRC.Model.AppContext
import CRC.Serve.Api (AppApi (..))
import CRC.Serve.AppHandler
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
    { createCheckRun = checkRunCreatorHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
