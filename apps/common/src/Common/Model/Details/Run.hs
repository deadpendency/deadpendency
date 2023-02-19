module Common.Model.Details.Run
  ( Run (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Details.RunTrace
import Common.Model.Git.GitRef
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import Common.Model.GitHub.GHRepoFullName
import Common.Model.GitHub.GHRepoOwnerType
import Common.Model.GitHub.GHUserName
import Data.Aeson

data Run = Run
  { _runTrace :: RunTrace,
    _gitRef :: Maybe GitRef,
    _gitHeadSha :: GitSha,
    _repoDependencyName :: GHRepoFullName,
    _repoPrivate :: Bool,
    _repoOwnerType :: GHRepoOwnerType,
    _repoOwnerAccountId :: Int,
    _qualifiedRepo :: QualifiedRepo,
    _repoNodeId :: GHNodeId,
    _triggeredUser :: GHUserName,
    _isDeadpendencyRun :: Bool,
    _appInstallationId :: GHAppInstallationId,
    _ghInstallationAuth :: Maybe GHInstallationAuth
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Run where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON Run where
  parseJSON = genericParseJSON cleanJSONOptions
