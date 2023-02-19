module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeJavaMaven (smokeJavaMaven) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUxNTQzODg0MTg0"
    PreProd -> "MDEwOkNoZWNrU3VpdGUxNTQzODg0MTg1"
    Test -> "MDEwOkNoZWNrU3VpdGUxNTQzODg0MTg1"

smokeJavaMaven :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeJavaMaven installAuth appEnv appId = do
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
            _commitSha = GitSha "28fed143ea79dab8c452c2dbacedd8b73c2b41c2",
            _expectedDeps = expectedDeps,
            _expectedErrors = expectedErroredDeps
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed $ DependencyName "com.h2database/h2",
    DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate-core",
    DependencyIdentifierNamed $ DependencyName "javax.servlet/javax.servlet-api",
    DependencyIdentifierNamed $ DependencyName "javax.servlet.jsp/javax.servlet.jsp-api",
    DependencyIdentifierNamed $ DependencyName "joda-time/joda-time",
    DependencyIdentifierNamed $ DependencyName "org.mockito/mockito-all",
    DependencyIdentifierNamed $ DependencyName "mysql/mysql-connector-java",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-core",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-orm",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-test",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-tx",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-web",
    DependencyIdentifierNamed $ DependencyName "org.springframework/spring-webmvc",
    DependencyIdentifierNamed $ DependencyName "org.testng/testng",
    DependencyIdentifierNamed $ DependencyName "javax.validation/validation-api",
    DependencyIdentifierNamed $ DependencyName "dbunit/dbunit",
    DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate-validator",
    DependencyIdentifierNamed $ DependencyName "javax.servlet/jstl",
    DependencyIdentifierNamed $ DependencyName "org.jadira.usertype/usertype.core"
  ]

expectedErroredDeps :: [DependencyIdentifier]
expectedErroredDeps = []
