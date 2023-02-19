module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.Packagist
  ( fetchDependencyPackagist,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.PackagistPackage
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Network.HTTP.Req

fetchDependencyPackagist :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyPackagist dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText <> ".json"
      url = https "packagist.org" /: "packages" /: dependencyNameText
  eitherMaybePackagistPackage <- fetchJSON @PackagistPackage url
  pure $ eitherMaybePackagistPackage <<&>> _result
