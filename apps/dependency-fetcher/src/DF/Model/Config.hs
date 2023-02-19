module DF.Model.Config
  ( Config (..),
  )
where

import Common.Model.GitHub.GHAppId

data Config = Config
  { _redisDatabaseHost :: Text,
    _githubPrivateKeySecretName :: Text,
    _appId :: GHAppId
  }
  deriving stock (Generic)
