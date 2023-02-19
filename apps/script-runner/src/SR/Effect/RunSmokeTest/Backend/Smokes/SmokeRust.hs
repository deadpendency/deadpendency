module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeRust (smokeRust) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxNDAzOTkyMjk3"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxNDAzOTkyMjk5"
    Test -> "MDEwOkNoZWNrU3VpdGUxNDAzOTkyMjk5"

smokeRust :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeRust installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkzMDc1MzQ3ODA="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-rust"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "8286353dde30774dc1f25ac6e7ab86f905c40820",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed
      (DependencyName {_ntText = "chrono"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "diesel"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "dotenv"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "jsonwebtoken"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "once_cell"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rand"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rocket"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rocket_contrib"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rocket_cors"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rust-crypto"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "serde"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "serde_json"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "slug"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "validator"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "validator_derive"})
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
