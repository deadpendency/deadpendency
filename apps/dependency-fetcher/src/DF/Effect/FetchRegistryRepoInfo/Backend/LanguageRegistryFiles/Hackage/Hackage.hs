module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.Hackage
  ( fetchDependencyHackage,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Ecosystem.Registry
import Control.Monad.Except (throwError)
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.HackageCabalRepo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.HackageDeprecation
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.HackageLatestRevision
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError

fetchDependencyHackage :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyHackage dependencyName = runExceptT $ do
  maybeHackageCabalRepo <- fetchCabalRepo dependencyName
  case maybeHackageCabalRepo of
    Just (HackageCabalRepo maybeRepo) -> do
      maybeLatestReleaseTime <- fetchLatestReleaseTime dependencyName
      maybePackageDeprecated <- fetchPackageDeprecation dependencyName
      case (maybeLatestReleaseTime, maybePackageDeprecated) of
        (Just latestReleaseTime, Just packageDeprecated) ->
          pure $ Just $ DependencyRegistryInfo Hackage maybeRepo packageDeprecated (Just latestReleaseTime)
        unexpected ->
          throwError $
            FDRRegistryDataInconsistent $
              "Unexpected combination of package data for " <> (dependencyName ^. #_ntText) <> " - " <> show unexpected
    Nothing -> pure Nothing
