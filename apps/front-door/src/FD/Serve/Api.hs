{-# LANGUAGE DataKinds #-}

module FD.Serve.Api
  ( AppApi (..),
  )
where

import FD.Handler.CheckRunHandler (CheckRunRoute)
import FD.Handler.CheckSuiteHandler (CheckSuiteRoute)
import FD.Handler.InstallationHandler (InstallationRoute)
import FD.Handler.InstallationRepositoriesHandler (InstallationRepositoriesRoute)
import FD.Handler.MarketplacePurchaseHandler (MarketplacePurchaseRoute)
import Servant.API.Generic

data AppApi route = AppApi
  { checkSuite ::
      route
        :- CheckSuiteRoute,
    checkRun ::
      route
        :- CheckRunRoute,
    installation ::
      route
        :- InstallationRoute,
    installationRepositories ::
      route
        :- InstallationRepositoriesRoute,
    marketplacePurchase ::
      route
        :- MarketplacePurchaseRoute
  }
  deriving stock (Generic)
