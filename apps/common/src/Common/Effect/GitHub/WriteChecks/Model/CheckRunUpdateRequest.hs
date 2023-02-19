module Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequest
  ( CheckRunUpdateRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequestStatus
import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.Output.CheckRunOutput
import Common.Model.GitHub.GHNodeId
import Data.Aeson

data CheckRunUpdateRequest = CheckRunUpdateRequest
  { _repoNodeId :: GHNodeId,
    _checkRunNodeId :: GHNodeId,
    _checkRunStatus :: Maybe CheckRunUpdateRequestStatus,
    _checkRunConclusion :: Maybe CheckRunConclusion,
    _checkRunOutput :: Maybe CheckRunOutput
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunUpdateRequest where
  toJSON = genericToJSON cleanJSONOptions
