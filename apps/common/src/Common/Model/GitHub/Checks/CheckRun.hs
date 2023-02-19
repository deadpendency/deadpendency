module Common.Model.GitHub.Checks.CheckRun
  ( CheckRun (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitSha
import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.CheckRunStatus
import Common.Model.GitHub.GHNodeId
import Data.Aeson

data CheckRun = CheckRun
  { _nodeId :: GHNodeId,
    _headSha :: GitSha,
    _name :: Text,
    _repoNodeId :: GHNodeId,
    _checkRunStatus :: CheckRunStatus,
    _checkRunConclusion :: Maybe CheckRunConclusion
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRun where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRun where
  parseJSON = genericParseJSON cleanJSONOptions
