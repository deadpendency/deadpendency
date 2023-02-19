module DF.Effect.FetchRegistryRepoInfo.Backend.RegistryFetchRegistryBackend
  ( fetchRegistryInfo,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Ecosystem.ProgrammingLanguage
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Crates.Crates
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.Hackage
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Maven
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.Npm
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGet
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.Packagist
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDev
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.Pypi
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGems
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError

fetchRegistryInfo :: ProgrammingLanguage -> DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchRegistryInfo programmingLanguage dependencyName = do
  result <-
    case programmingLanguage of
      JavaScript -> fetchDependencyNPM dependencyName
      TypeScript -> fetchDependencyNPM dependencyName
      Python -> fetchDependencyPypi dependencyName
      Php -> fetchDependencyPackagist dependencyName
      Ruby -> fetchDependencyRubyGems dependencyName
      Haskell -> fetchDependencyHackage dependencyName
      Rust -> fetchDependencyCrates dependencyName
      CSharpNet -> fetchDependencyNuGet dependencyName
      VisualBasicNet -> fetchDependencyNuGet dependencyName
      Java -> fetchDependencyMaven Java dependencyName
      Kotlin -> fetchDependencyMaven Kotlin dependencyName
      Scala -> fetchDependencyMaven Scala dependencyName
      Golang -> fetchDependencyPkgGoDev dependencyName
      UnsupportedLanguage _ -> pure $ Right Nothing

  evaluateWHNF result
