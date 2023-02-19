{-# LANGUAGE DataKinds #-}

module CRC.Serve.Api
  ( AppApi (..),
  )
where

import CRC.Handler.CheckRunCreatorHandler (CheckRunCreatorRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { createCheckRun ::
      route
        :- CheckRunCreatorRoute
  }
  deriving stock (Generic)
