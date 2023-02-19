module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.Pypi
  ( fetchDependencyPypi,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.PypiPackage
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Network.HTTP.Req

fetchDependencyPypi :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyPypi dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText
      url = https "pypi.org" /: "pypi" /: dependencyNameText /: "json"
  eitherMaybePypiPackage <- fetchJSON @PypiPackage url
  pure $ eitherMaybePypiPackage <<&>> _result
