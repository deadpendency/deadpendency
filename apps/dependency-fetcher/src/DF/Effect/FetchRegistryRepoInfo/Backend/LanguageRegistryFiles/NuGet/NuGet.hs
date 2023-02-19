module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGet
  ( fetchDependencyNuGet,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Ecosystem.Registry
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetAdditionalDetails
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetLatestVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetRepository
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError

{-
A nuance of this is that it only gets versions that are not development versions. This may be wrong for some packages that have no published releases yet, but saves a bunch of complexity for now and probably not an issue in 99% of cases.
-}

fetchDependencyNuGet :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyNuGet dependencyName = runExceptT $ do
  maybeLatestVersion <- fetchLatestVersion dependencyName
  case maybeLatestVersion of
    Nothing -> pure Nothing
    Just latestVersion -> do
      maybeRepository <- fetchSourceRepository latestVersion dependencyName
      additionalDetails <- fetchAdditionalDetails latestVersion dependencyName
      pure $
        Just $
          DependencyRegistryInfo
            { _registry = NuGet,
              _sourceRepo = maybeRepository,
              _alivenessStatus = additionalDetails ^. #_deprecated,
              _lastReleaseDateTime = additionalDetails ^. #_maybeReleaseDateTime
            }
