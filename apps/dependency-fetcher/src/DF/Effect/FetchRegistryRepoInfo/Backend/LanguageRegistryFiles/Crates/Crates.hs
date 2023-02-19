module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Crates.Crates
  ( fetchDependencyCrates,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Crates.CratesPackage
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Network.HTTP.Req

fetchDependencyCrates :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyCrates dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText
      url = https "crates.io" /: "api" /: "v1" /: "crates" /: dependencyNameText
  eitherMaybeCratesPackage <- fetchJSON @CratesPackage url
  pure $ eitherMaybeCratesPackage <<&>> _result
