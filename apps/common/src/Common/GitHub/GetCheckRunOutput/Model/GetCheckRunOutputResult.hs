module Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputResult
  ( GetCheckRunOutputResult (..),
  )
where

import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.CheckRunStatus
import Common.Model.GitHub.Checks.Output.CheckRunOutput
import Common.Model.GitHub.GHNodeId

data GetCheckRunOutputResult = GetCheckRunOutputResult
  { _checkRunId :: GHNodeId,
    _output :: Maybe CheckRunOutput,
    _status :: CheckRunStatus,
    _conclusion :: Maybe CheckRunConclusion
  }
  deriving stock (Eq, Show, Generic)
