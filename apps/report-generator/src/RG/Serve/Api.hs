{-# LANGUAGE DataKinds #-}

module RG.Serve.Api
  ( AppApi (..),
  )
where

import RG.Handler.ReportGeneratorHandler (ReportGeneratorRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { reportGenerator ::
      route
        :- ReportGeneratorRoute
  }
  deriving stock (Generic)
