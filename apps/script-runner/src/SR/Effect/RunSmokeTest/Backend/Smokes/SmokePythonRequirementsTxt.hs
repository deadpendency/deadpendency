module SR.Effect.RunSmokeTest.Backend.Smokes.SmokePythonRequirementsTxt (smokePythonRequirementsTxt) where

import Common.Model.Config.AppEnv
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppId
import Common.Model.GitHub.GHNodeId
import SR.Effect.RunSmokeTest.Backend.Model.SmokeConfiguration
import SR.Effect.RunSmokeTest.Backend.SmokeRepository
import SR.Effect.RunSmokeTest.Model.SmokeResult

getCheckSuiteNodeId :: AppEnv -> Text
getCheckSuiteNodeId =
  \case
    Prod -> "MDEwOkNoZWNrU3VpdGUxMDc5MTU0NTYy"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxMDc5MTU0NTYz"
    Test -> "MDEwOkNoZWNrU3VpdGUxMDc5MTU0NTYz"

smokePythonRequirementsTxt :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokePythonRequirementsTxt installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyNjQ4NTg0Nzk="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-python"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "f05064109e1e8289da6a6ede371cd4929e706679",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed
      (DependencyName {_ntText = "confuse"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "django"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "nose"})
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
