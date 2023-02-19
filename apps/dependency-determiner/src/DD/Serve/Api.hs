{-# LANGUAGE DataKinds #-}

module DD.Serve.Api
  ( AppApi (..),
  )
where

import DD.Handler.DependencyDeterminerHandler (DependencyDeterminerRoute)
import Servant.API.Generic ((:-))

newtype AppApi route = AppApi
  { dependencyDeterminer ::
      route
        :- DependencyDeterminerRoute
  }
  deriving stock (Generic)
