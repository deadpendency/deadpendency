module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Maven
  ( fetchDependencyMaven,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Ecosystem.Registry
import Control.Monad.Except (throwError)
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenLatestReleaseTime
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenRepositoryAndAliveness
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Version.MavenLatestVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Text qualified as Text

{-
A nuance of this is that it only gets versions that are not development versions. This may be wrong for some packages that have no published releases yet, but saves a bunch of complexity for now and probably not an issue in 99% of cases.
-}

fetchDependencyMaven :: ProgrammingLanguage -> DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyMaven programmingLanguage dependencyName = runExceptT $ do
  let dependencyNameText = dependencyName ^. #_ntText
      namespaceAndName = Text.splitOn "/" dependencyNameText
  (namespace, name) <-
    case namespaceAndName of
      [namespace, name] -> pure (namespace, name)
      _ -> throwError $ FDRDependencyNameInvalid $ "Missing slash in maven dep: " <> dependencyNameText
  maybeLatestVersion <- fetchLatestVersion programmingLanguage namespace name
  case maybeLatestVersion of
    Nothing -> pure Nothing
    Just latestVersion -> do
      (MavenRepositoryAndAliveness aliveness maybeRepository) <- fetchRepositoryAndAliveness namespace name latestVersion
      maybeReleaseTime <-
        case aliveness of
          RASDeprecated {} -> pure Nothing
          _ -> fetchLatestReleaseTime namespace name latestVersion <&> \rt -> rt ^? #_time
      pure $
        Just $
          DependencyRegistryInfo
            { _registry = Maven,
              _sourceRepo = maybeRepository,
              _alivenessStatus = aliveness,
              _lastReleaseDateTime = maybeReleaseTime
            }
