module RP.Model.AppContext
  ( AppContext (..),
  )
where

import Common.Model.Cache.CacheConfigGlobal
import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Details.ComponentDetails
import Common.Model.GitHub.Auth.GHAppAuthGlobal
import Network.Google qualified as G
import RP.AppGoogleScopes (AppGoogleScopes)

data AppContext = AppContext
  { _googleEnv :: G.Env AppGoogleScopes,
    _commonConfig :: CommonConfig,
    _instanceConfig :: InstanceConfig,
    _componentDetails :: ComponentDetails,
    _ghAppAuthGlobal :: GHAppAuthGlobal,
    _cacheConfigGlobal :: CacheConfigGlobal
  }
  deriving stock (Generic)
