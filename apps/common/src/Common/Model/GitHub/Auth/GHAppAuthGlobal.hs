module Common.Model.GitHub.Auth.GHAppAuthGlobal
  ( GHAppAuthGlobal (..),
  )
where

import Common.Model.GitHub.Auth.GHAppAuthPrereqs
import Common.Model.GitHub.Auth.GHSharedAppAuth
import Control.Concurrent (MVar)

data GHAppAuthGlobal = GHAppAuthGlobal
  { _appAuthPrereqs :: GHAppAuthPrereqs,
    _loadedAppAuth :: MVar (Maybe GHSharedAppAuth)
  }
  deriving stock (Generic)
