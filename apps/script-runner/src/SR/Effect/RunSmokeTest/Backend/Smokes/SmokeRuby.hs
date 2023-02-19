module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeRuby (smokeRuby) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxMDc5MTczNzQ5"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxMDc5MTczNzUw"
    Test -> "MDEwOkNoZWNrU3VpdGUxMDc5MTczNzUw"

smokeRuby :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeRuby installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyNzIzMzA5NTg="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-ruby"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "d787bb3924d36709351803b8b2ceceb3b29e5911",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed $ DependencyName "@rails/actioncable",
    DependencyIdentifierNamed $ DependencyName "@rails/activestorage",
    DependencyIdentifierNamed $ DependencyName "@rails/ujs",
    DependencyIdentifierNamed
      (DependencyName {_ntText = "bootsnap"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "bundler"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "byebug"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "capybara"}),
    DependencyIdentifierRepo
      ( QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner {_ntText = "bmabey"},
            _repoName = RepoName {_ntText = "database_cleaner"}
          }
      )
      ( Just
          (DependencyName {_ntText = "database_cleaner"})
      ),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "jbuilder"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "listen"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "parallel"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "parser"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "puma"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rails"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rainbow"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "regexp_parser"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rexml"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "rubocop-ast"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "ruby-progressbar"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "sass-rails"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "selenium-webdriver"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "spring"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "spring-watcher-listen"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "sqlite3"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "turbolinks"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "turbolinks"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "tzinfo-data"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "unicode-display_width"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "web-console"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "webdrivers"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "webpacker"})
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
