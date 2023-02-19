module FD.Serve.Server
  ( runServer,
  )
where

import FD.Handler.CheckRunHandler (checkRunHandler)
import FD.Handler.CheckSuiteHandler (checkSuiteHandler)
import FD.Handler.InstallationHandler (installationHandler)
import FD.Handler.InstallationRepositoriesHandler (installationRepositoriesHandler)
import FD.Handler.MarketplacePurchaseHandler (marketplacePurchaseHandler)
import FD.Model.AppContext
import FD.Serve.Api (AppApi (..))
import FD.Serve.AppHandler (CarrierStack, appToHandler)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context (..))
import Servant.Server.Generic (AsServerT, genericServeTWithContext)

webAppGeneric :: AppContext -> Wai.Application
webAppGeneric appContext =
  let gitHubKey = appContext ^. #_loadedGitHubKey
   in genericServeTWithContext
        (appToHandler appContext)
        api
        (gitHubKey :. EmptyContext)

api :: AppApi (AsServerT CarrierStack)
api =
  AppApi
    { checkSuite = checkSuiteHandler,
      checkRun = checkRunHandler,
      installation = installationHandler,
      installationRepositories = installationRepositoriesHandler,
      marketplacePurchase = marketplacePurchaseHandler
    }

runServer :: AppContext -> IO ()
runServer appContext =
  let port = appContext ^. (#_commonConfig . #_port)
   in Warp.run port (webAppGeneric appContext)
