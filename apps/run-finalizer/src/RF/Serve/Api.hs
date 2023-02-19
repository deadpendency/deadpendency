{-# LANGUAGE DataKinds #-}

module RF.Serve.Api
  ( AppApi (..),
  )
where

import RF.Handler.RunFinalizerHandler (RunFinalizerRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { runFinalizer ::
      route
        :- RunFinalizerRoute
  }
  deriving stock (Generic)
