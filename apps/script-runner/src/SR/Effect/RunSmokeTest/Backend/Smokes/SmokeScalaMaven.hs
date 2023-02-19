module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeScalaMaven (smokeScalaMaven) where

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
    Prod -> "CS_kwDOHwUhps8AAAABx3H2LQ"
    PreProd -> "CS_kwDOHwUhps8AAAABx3H2Ng"
    Test -> "CS_kwDOHwUhps8AAAABx3H2Ng"

smokeScalaMaven :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeScalaMaven installAuth appEnv appId = do
  let repoNodeId = GHNodeId "R_kgDOHwUhpg"
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-scala"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "4e9b41c4ffb4d629857be7fdd1d2640f27d1bd69",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed $ DependencyName "com.storm-enroute/scalameter",
    DependencyIdentifierNamed $ DependencyName "org.scalaj/scalaj-http",
    DependencyIdentifierNamed $ DependencyName "org.scalatest/scalatest",
    DependencyIdentifierNamed $ DependencyName "org.typelevel/cats-core",
    DependencyIdentifierNamed $ DependencyName "org.typelevel/cats-effect",
    DependencyIdentifierNamed $ DependencyName "org.typelevel/cats-macros"
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
