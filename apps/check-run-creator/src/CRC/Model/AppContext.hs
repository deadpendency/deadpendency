module CRC.Model.AppContext
  ( AppContext (..),
  )
where

import CRC.AppGoogleScopes (AppGoogleScopes)
import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Details.ComponentDetails
import Common.Model.GitHub.Auth.GHAppAuthGlobal
import Network.Google qualified as G

data AppContext = AppContext
  { _googleEnv :: G.Env AppGoogleScopes,
    _commonConfig :: CommonConfig,
    _instanceConfig :: InstanceConfig,
    _componentDetails :: ComponentDetails,
    _ghAppAuthGlobal :: GHAppAuthGlobal
  }
  deriving stock (Generic)
