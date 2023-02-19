module SR.Model.AppContext
  ( AppContext (..),
  )
where

import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Details.ComponentDetails
import Common.Model.GitHub.Auth.GHAppAuthGlobal
import Network.Google qualified as G
import SR.AppGoogleScopes (AppGoogleScopes)
import SR.Model.Config

data AppContext = AppContext
  { _googleEnv :: G.Env AppGoogleScopes,
    _commonConfig :: CommonConfig,
    _instanceConfig :: InstanceConfig,
    _componentDetails :: ComponentDetails,
    _appConfig :: Config,
    _ghAppAuthGlobal :: GHAppAuthGlobal
  }
  deriving stock (Generic)
