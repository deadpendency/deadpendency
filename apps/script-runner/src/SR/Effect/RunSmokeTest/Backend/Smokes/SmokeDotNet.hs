module SR.Effect.RunSmokeTest.Backend.Smokes.SmokeDotNet (smokeDotNet) where

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
    Prod -> "MDEwOkNoZWNrU3VpdGUzNTU2Mzg1MzIz"
    PreProd -> "MDEwOkNoZWNrU3VpdGUzNTU2Mzg1MzI2"
    Test -> "MDEwOkNoZWNrU3VpdGUzNTU2Mzg1MzI2"

smokeDotNet :: GHInstallationAuth -> AppEnv -> GHAppId -> IO SmokeResult
smokeDotNet installAuth appEnv appId = do
  let repoNodeId = GHNodeId "MDEwOlJlcG9zaXRvcnkzMDkyMjA3MjY="
      checkSuiteNodeId = GHNodeId $ getCheckSuiteNodeId appEnv
      qualifiedRepo =
        QualifiedRepo
          { _repoHost = GitHub,
            _repoOwner = RepoOwner "deadpendency",
            _repoName = RepoName "smoke-dotnet"
          }
      smokeConfiguration =
        SmokeConfiguration
          { _repoNodeId = repoNodeId,
            _checkSuiteNodeId = checkSuiteNodeId,
            _repo = qualifiedRepo,
            _commitSha = GitSha "18c27f782f2a8c80401eefb9aa3411771be12dd0",
            _expectedDeps = expectedDeps,
            _expectedErrors = []
          }

  smokeRepository appId installAuth smokeConfiguration

expectedDeps :: [DependencyIdentifier]
expectedDeps =
  [ DependencyIdentifierNamed
      (DependencyName {_ntText = "Ardalis.ApiEndpoints"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Ardalis.EFCore.Extensions"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Ardalis.GuardClauses"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Ardalis.Specification"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Ardalis.Specification.EntityFrameworkCore"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "AutoMapper.Extensions.Microsoft.DependencyInjection"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Blazored.LocalStorage"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "MediatR"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "MediatR.Extensions.Microsoft.DependencyInjection"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Authentication.JwtBearer"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Components.Authorization"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Components.WebAssembly"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Components.WebAssembly.Authentication"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Components.WebAssembly.Build"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Components.WebAssembly.DevServer"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Diagnostics.EntityFrameworkCore"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Identity"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Identity.EntityFrameworkCore"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.AspNetCore.Identity.UI"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.EntityFrameworkCore.InMemory"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.EntityFrameworkCore.SqlServer"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.EntityFrameworkCore.Tools"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.Extensions.Identity.Core"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.Extensions.Logging.Configuration"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.VisualStudio.Web.CodeGeneration.Design"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Swashbuckle.AspNetCore"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Swashbuckle.AspNetCore.Annotations"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Swashbuckle.AspNetCore.SwaggerUI"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "System.IdentityModel.Tokens.Jwt"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "System.Net.Http.Json"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "System.Text.Json"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "BlazorInputFile"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "Microsoft.VisualStudio.Azure.Containers.Tools.Targets"}),
    DependencyIdentifierNamed
      (DependencyName {_ntText = "System.Security.Claims"})
  ]
