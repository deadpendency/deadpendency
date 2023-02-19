module SR.Effect.RunSmokeTest.Backend.Model.SmokeConfiguration (SmokeConfiguration (..)) where

import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.GHNodeId

data SmokeConfiguration = SmokeConfiguration
  { _repoNodeId :: GHNodeId,
    _checkSuiteNodeId :: GHNodeId,
    _repo :: QualifiedRepo,
    _commitSha :: GitSha,
    _expectedDeps :: [DependencyIdentifier],
    _expectedErrors :: [DependencyIdentifier]
  }
  deriving stock (Generic)
