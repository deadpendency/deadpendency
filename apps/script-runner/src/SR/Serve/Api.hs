{-# LANGUAGE DataKinds #-}

module SR.Serve.Api
  ( AppApi (..),
  )
where

import SR.Handler.EmitTotalInstallsMetricHandler (EmitTotalInstallsRoute)
import SR.Handler.ReplayFailedHandler (ReplayFailedRoute)
import SR.Handler.RunSmokeTestHandler (RunSmokeTestRoute)
import Servant.API.Generic ((:-))

data AppApi route = AppApi
  { replayFailed ::
      route
        :- ReplayFailedRoute,
    runSmokeTest ::
      route
        :- RunSmokeTestRoute,
    emitTotalInstalls ::
      route
        :- EmitTotalInstallsRoute
  }
  deriving stock (Generic)
