module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeGolang (smokeGolang) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUyOTIyMzg0MTgw"
    PreProd -> "MDEwOkNoZWNrU3VpdGUyOTIyMzg0MTgy"
    Test -> "MDEwOkNoZWNrU3VpdGUyOTIyMzg0MTgy"

smokeGolang :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeGolang installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkzNzQzMTEzMjU="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-go"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "62d33523e4896645dba4ee969489113c806935c0",
            _expectedDeps = expectedDeps,
            _expectedErrors = []
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed (DependencyName "github.com/gorilla/mux"),
    DependencyIdentifierNamed (DependencyName "github.com/sirupsen/logrus"),
    DependencyIdentifierNamed (DependencyName "gopkg.in/alecthomas/kingpin.v2")
  ]
