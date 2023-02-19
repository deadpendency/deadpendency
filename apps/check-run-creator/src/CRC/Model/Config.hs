module CRC.Model.Config
  ( Config (..),
  )
where

import Common.Model.GitHub.GHAppId

data Config = Config
  { _githubPrivateKeySecretName :: Text,
    _appId :: GHAppId
  }
  deriving stock (Generic)
