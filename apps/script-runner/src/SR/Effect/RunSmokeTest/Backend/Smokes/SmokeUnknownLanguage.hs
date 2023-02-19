module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeUnknownLanguage (smokeUnknownLanguage) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUzNTU2MzgxOTIw"
    PreProd -> "MDEwOkNoZWNrU3VpdGUzNTU2MzgxOTI2"
    Test -> "MDEwOkNoZWNrU3VpdGUzNTU2MzgxOTI2"

smokeUnknownLanguage :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeUnknownLanguage installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyNzI4ODQ5MDQ="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-unknown-language"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "5c242a04a55521433cf4d68c675dbf7b0fa58cb8",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierRepo
      ( QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner {_ntText = "commercialhaskell"},
            _repoName = RepoName {_ntText = "stack"}
          }
      )
      (Just (DependencyName {_ntText = "stack"})),
    DependencyIdentifierRepo
      ( QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner {_ntText = "AlistairB"},
            _repoName = RepoName {_ntText = "an-archived-repo"}
          }
      )
      Nothing,
    DependencyIdentifierRepo
      ( QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner {_ntText = "AlistairB"},
            _repoName = RepoName {_ntText = "morpheus-graphql"}
          }
      )
      Nothing
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
