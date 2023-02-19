module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeHaskell (smokeHaskell) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxNDY3MDEzNzA2"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxNDY3MDEzNzA3"
    Test -> "MDEwOkNoZWNrU3VpdGUxNDY3MDEzNzA3"

smokeHaskell :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeHaskell installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkzMTA0ODQ3ODI="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-haskell"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "0574438b53e9bcdba04bd8ef48444cb83379c597",
            _expectedDeps = expectedDeps,
            _expectedErrors = []
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed
      (DependencyName {_ntText = "aeson"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "bytestring"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "hspec"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "hspec-wai"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "hspec-wai-json"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "relude"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "servant-server"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "text"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "wai"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "warp"})
  ]
