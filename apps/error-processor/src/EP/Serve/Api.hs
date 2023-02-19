{-# LANGUAGE DataKinds #-}

module EP.Serve.Api
  ( AppApi (..),
  )
where

import EP.Handler.PublishFailedReportHandler (PublishFailedReportRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { publishFailedReport ::
      route
        :- PublishFailedReportRoute
  }
  deriving stock (Generic)
