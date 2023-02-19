module Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
  ( CheckRunCreateRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitSha
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import Data.Aeson

data CheckRunCreateRequest = CheckRunCreateRequest
  { _headSha :: GitSha,
    _name :: Text,
    _repoNodeId :: GHNodeId,
    _appInstallationId :: GHAppInstallationId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunCreateRequest where
  toJSON = genericToJSON cleanJSONOptions
