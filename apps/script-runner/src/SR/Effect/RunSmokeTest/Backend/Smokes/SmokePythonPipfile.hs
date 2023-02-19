module SR.Effect.RunSmokeTest.Backend.Smokes.SmokePythonPipfile (smokePythonPipfile) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxMDg1NTY4Nzk3"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxMDg1NTY4Nzk4"
    Test -> "MDEwOkNoZWNrU3VpdGUxMDg1NTY4Nzk4"

smokePythonPipfile :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokePythonPipfile installAuth appEnv appId = do
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
            _commitSha = GitSha "796f79b37cf30b1ccb2fe52c3c60591914d7a8d8",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierRepo
      ( QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner {_ntText = "django"},
            _repoName = RepoName {_ntText = "django"}
          }
      )
      ( Just
          (DependencyName {_ntText = "django"})
      ),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "pywinusb"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "records"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "requests"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "nose"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "unittest2"})
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps =
  [ DependencyIdentifierNamed
      (DependencyName {_ntText = "e1839a8"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "e682b37"})
  ]
