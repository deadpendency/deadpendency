{-# LANGUAGE DataKinds #-}

module DF.Serve.Api
  ( AppApi (..),
  )
where

import DF.Handler.DependencyFetcherHandler (DependencyFetcherRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { dependencyFetcher ::
      route
        :- DependencyFetcherRoute
  }
  deriving stock (Generic)
