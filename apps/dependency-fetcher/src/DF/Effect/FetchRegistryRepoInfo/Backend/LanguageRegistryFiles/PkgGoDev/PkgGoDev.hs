module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDev
  ( fetchDependencyPkgGoDev,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.Repo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevLatestReleaseTime
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevRepository
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError

fetchDependencyPkgGoDev :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyPkgGoDev dependencyName = runExceptT $ do
  let maybeRepository = determineSourceRepository dependencyName
  (PkgGoDevLatestReleaseTime maybeLatestReleaseTime) <- fetchLatestReleaseTime dependencyName
  pure $
    case (maybeLatestReleaseTime, maybeRepository) of
      (Nothing, Nothing) -> Nothing
      _ ->
        Just $
          DependencyRegistryInfo
            { _registry = PkgGoDev,
              _sourceRepo = RepoQR <$> maybeRepository,
              _alivenessStatus = RASAlive,
              _lastReleaseDateTime = maybeLatestReleaseTime
            }
