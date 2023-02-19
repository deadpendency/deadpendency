module RG.Model.AppContext
  ( AppContext (..),
  )
where

import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Details.ComponentDetails
import Network.Google qualified as G
import RG.AppGoogleScopes (AppGoogleScopes)

data AppContext = AppContext
  { _googleEnv :: G.Env AppGoogleScopes,
    _commonConfig :: CommonConfig,
    _instanceConfig :: InstanceConfig,
    _componentDetails :: ComponentDetails
  }
  deriving stock (Generic)
