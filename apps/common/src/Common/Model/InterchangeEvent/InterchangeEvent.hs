module Common.Model.InterchangeEvent.InterchangeEvent
  ( InterchangeEvent (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Details.Run
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.RepoConfig.RepoConfig
import Data.Aeson

data InterchangeEvent a = InterchangeEvent
  { _ghInstallationAuth :: GHInstallationAuth,
    _checkRun :: CheckRun,
    _run :: Run,
    _repoConfig :: RepoConfig,
    _knownFailureOccurred :: Bool,
    _result :: a
  }
  deriving stock (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (InterchangeEvent a) where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance (FromJSON a) => FromJSON (InterchangeEvent a) where
  parseJSON = genericParseJSON cleanJSONOptions
