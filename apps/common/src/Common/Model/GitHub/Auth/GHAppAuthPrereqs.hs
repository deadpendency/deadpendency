module Common.Model.GitHub.Auth.GHAppAuthPrereqs
  ( GHAppAuthPrereqs (..),
  )
where

import Common.Model.GitHub.GHAppId
import Crypto.PubKey.RSA (PrivateKey)

data GHAppAuthPrereqs = GHAppAuthPrereqs
  { _appId :: GHAppId,
    _privateKey :: PrivateKey
  }
  deriving stock (Generic)
