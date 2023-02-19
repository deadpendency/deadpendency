module SR.Effect.RunSmokeTest.Backend.Smokes.SmokePhp (smokePhp) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxMDc5MDY4MDEw"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxMDc5MDY4MDEx"
    Test -> "MDEwOkNoZWNrU3VpdGUxMDc5MDY4MDEx"

smokePhp :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokePhp installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkyNzE0Mzg5MzY="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-php"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "3756bc58d810bf4573a3e1f776c8ffe43bd81d80",
            _expectedDeps = expectedDeps,
            _expectedErrors = []
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed (DependencyName "aws/aws-sdk-php"),
    DependencyIdentifierNamed (DependencyName "doctrine/couchdb"),
    DependencyIdentifierNamed (DependencyName "elasticsearch/elasticsearch"),
    DependencyIdentifierNamed (DependencyName "graylog2/gelf-php"),
    DependencyIdentifierNamed (DependencyName "php-amqplib/php-amqplib"),
    DependencyIdentifierNamed (DependencyName "php-console/php-console"),
    DependencyIdentifierNamed (DependencyName "php-parallel-lint/php-parallel-lint"),
    DependencyIdentifierNamed (DependencyName "phpspec/prophecy"),
    DependencyIdentifierNamed (DependencyName "phpunit/phpunit"),
    DependencyIdentifierNamed (DependencyName "predis/predis"),
    DependencyIdentifierNamed (DependencyName "psr/log"),
    DependencyIdentifierNamed (DependencyName "rollbar/rollbar"),
    DependencyIdentifierNamed (DependencyName "ruflin/elastica"),
    DependencyIdentifierNamed (DependencyName "swiftmailer/swiftmailer")
  ]
