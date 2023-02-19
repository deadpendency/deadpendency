module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeJavaScript (smokeJavaScript) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUzNTU2Mzc0NDI1"
    PreProd -> "MDEwOkNoZWNrU3VpdGUzNTU2Mzc0NDI2"
    Test -> "MDEwOkNoZWNrU3VpdGUzNTU2Mzc0NDI2"

smokeJavaScript :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeJavaScript installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyNjMyMDU0OTc="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-javascript"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "afb4932fbee6f6126b3ee9515f50683c9812dd76",
            _expectedDeps = expectedDeps,
            _expectedErrors = []
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed $ DependencyName "@testing-library/jest-dom",
    DependencyIdentifierNamed $ DependencyName "@testing-library/react",
    DependencyIdentifierNamed $ DependencyName "@testing-library/user-event",
    DependencyIdentifierNamed
      (DependencyName {_ntText = "lodash"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "nomnom"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "react"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "react-scripts"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "underscore"}),
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
      Nothing,
    DependencyIdentifierRepo
      ( QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner {_ntText = "commercialhaskell"},
            _repoName = RepoName {_ntText = "stack"}
          }
      )
      Nothing
  ]
