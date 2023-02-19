{-# LANGUAGE DataKinds #-}

module RP.Serve.Api
  ( AppApi (..),
  )
where

import RP.Handler.RunPreparerHandler (RunPreparerRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { prepareRun ::
      route
        :- RunPreparerRoute
  }
  deriving stock (Generic)
