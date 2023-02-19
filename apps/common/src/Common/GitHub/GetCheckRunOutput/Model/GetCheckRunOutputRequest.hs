module Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputRequest
  ( GetCheckRunOutputRequest (..),
  )
where

import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo

data GetCheckRunOutputRequest = GetCheckRunOutputRequest
  { _appId :: Int,
    _qualifiedRepo :: QualifiedRepo,
    _commitSha :: GitSha
  }
  deriving stock (Eq, Show, Generic)
