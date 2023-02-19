module DF.Effect.FetchRegistryRepoInfo.Backend.RegistryFetchOrchestrateBackend
  ( getRegistryInfo,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Ecosystem.ProgrammingLanguage
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchRegistryRepoInfo.Backend.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import DF.Effect.FetchRegistryRepoInfo.Backend.RegistryFetchRegistryBackend
import DF.Effect.FetchRegistryRepoInfo.Model.FetchRegistryRepoInfoResult

getRegistryInfo :: BasicDependency -> IO (Either FetchDependenciesError FetchRegistryRepoInfoResult)
getRegistryInfo basicDependency = do
  let dependencyIdentifier = basicDependency ^. #_dependencyIdentifier
      programmingLanguage = basicDependency ^. #_programmingLanguage
  eitherRegistryInfo <- getRegistryInfoInternal programmingLanguage dependencyIdentifier

  pure $
    produceResult basicDependency eitherRegistryInfo

getRegistryInfoInternal :: ProgrammingLanguage -> DependencyIdentifier -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
getRegistryInfoInternal programmingLanguage dependencyIdentifier =
  case dependencyIdentifier of
    DependencyIdentifierRepo _ Nothing -> pure $ Right Nothing
    DependencyIdentifierRepo _ (Just dependencyName) -> fetchRegistryInfo programmingLanguage dependencyName
    DependencyIdentifierNamed dependencyName -> fetchRegistryInfo programmingLanguage dependencyName
