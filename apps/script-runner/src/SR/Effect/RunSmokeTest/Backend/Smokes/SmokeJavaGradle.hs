module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeJavaGradle (smokeJavaGradle) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxNTc5ODk2OTc4"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxNTc5ODk2OTgx"
    Test -> "MDEwOkNoZWNrU3VpdGUxNTc5ODk2OTgx"

smokeJavaGradle :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeJavaGradle installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkzMTUxNTQ3Nzg="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-java"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "d1652a05e03852be3442067e570f281257a64693",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed $ DependencyName "commons-codec/commons-codec",
    DependencyIdentifierNamed $ DependencyName "com.h2database/h2",
    DependencyIdentifierNamed $ DependencyName "net.bull.javamelody/javamelody-core",
    DependencyIdentifierNamed $ DependencyName "org.mindrot/jbcrypt",
    DependencyIdentifierNamed $ DependencyName "junit/junit",
    DependencyIdentifierNamed $ DependencyName "org.neo4j/neo4j-jmx",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-web",
    DependencyIdentifierNamed $ DependencyName "org.apache.kafka/kafka_2.11",
    DependencyIdentifierNamed $ DependencyName "org.keycloak/keycloak-saml-core",
    DependencyIdentifierNamed $ DependencyName "org.apache.sling/org.apache.sling.engine",
    DependencyIdentifierNamed $ DependencyName "com.orientechnologies/orientdb-server"
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
