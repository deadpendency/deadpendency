module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.Npm
  ( fetchDependencyNPM,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.NpmPackage
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Network.HTTP.Req

fetchDependencyNPM :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyNPM dependencyName = runExceptT $ do
  let dependencyNameText = dependencyName ^. #_ntText
      fullUrl = https "registry.npmjs.org" /: dependencyNameText
  maybeBSResult <- ExceptT $ fetchUrl' fullUrl
  hoistEither $ _result <<$>> for maybeBSResult (parseNpmPackage dependencyName)
