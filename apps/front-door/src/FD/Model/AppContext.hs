module FD.Model.AppContext
  ( AppContext (..),
  )
where

import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Details.ComponentDetails
import FD.AppGoogleScopes (AppGoogleScopes)
import FD.Model.Config
import FD.Model.LoadedGitHubKey
import Network.Google qualified as G

data AppContext = AppContext
  { _googleEnv :: G.Env AppGoogleScopes,
    _config :: Config,
    _commonConfig :: CommonConfig,
    _instanceConfig :: InstanceConfig,
    _componentDetails :: ComponentDetails,
    _loadedGitHubKey :: LoadedGitHubKey
  }
  deriving stock (Generic)
