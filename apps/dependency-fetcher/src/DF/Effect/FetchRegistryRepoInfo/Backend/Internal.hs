module DF.Effect.FetchRegistryRepoInfo.Backend.Internal
  ( produceResult,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchDependencies.Model.FetchDependencyRegistryException
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import DF.Effect.FetchRegistryRepoInfo.Model.FetchRegistryRepoInfoResult
import DF.Effect.Model.FetchRegistryWithRepo

produceResult :: BasicDependency -> Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo) -> Either FetchDependenciesError FetchRegistryRepoInfoResult
produceResult basicDependency eitherRegistryInfo =
  let dependencyIdentifier = basicDependency ^. #_dependencyIdentifier
      dependencyType = basicDependency ^. #_dependencyType
      programmingLanguage = basicDependency ^. #_programmingLanguage
   in case eitherRegistryInfo of
        Left (FDRFailureToParseResult errorText) ->
          Right $ Left $ ErroredDependency dependencyIdentifier dependencyType programmingLanguage Nothing (UnexpectedFailureToParseRegistryEntry errorText)
        Left (FDRRegistryDataInconsistent errorText) ->
          Right $ Left $ ErroredDependency dependencyIdentifier dependencyType programmingLanguage Nothing (UnexpectedFailureRegistryDataInconsistent errorText)
        Left (FDRDependencyNameInvalid errorText) ->
          Right $ Left $ ErroredDependency dependencyIdentifier dependencyType programmingLanguage Nothing (UnexpectedDependencyNameInvalid errorText)
        Left (FDRRegistryFetchExceptional errorText) ->
          Left $ RegistryFetchError $ FDRERegistryFetchExceptional errorText
        Right maybeRegistryInfo ->
          let maybeRepo = getQualifiedRepo dependencyIdentifier maybeRegistryInfo
           in case (maybeRepo, maybeRegistryInfo) of
                (Nothing, Nothing) -> Right $ Left $ ErroredDependency dependencyIdentifier dependencyType programmingLanguage Nothing NoRegistryOrRepoData
                (Nothing, Just registryInfo) -> Right $ Right $ FetchRegistryWithRepo basicDependency $ This registryInfo
                (Just repo@(RepoQR (QualifiedRepo GitHub _ _)), _) -> Right $ Right $ FetchRegistryWithRepo basicDependency $ thatMaybeThese maybeRegistryInfo repo
                (Just _, Just registryInfo) -> Right $ Right $ FetchRegistryWithRepo basicDependency $ This registryInfo
                (Just _, Nothing) -> Right $ Left $ ErroredDependency dependencyIdentifier dependencyType programmingLanguage Nothing NoRegistryOrRepoData

getQualifiedRepo :: DependencyIdentifier -> Maybe DependencyRegistryInfo -> Maybe Repo
getQualifiedRepo dependencyIdentifier maybeRegistryInfo =
  case (dependencyIdentifier, maybeRegistryInfo) of
    (DependencyIdentifierRepo qualifiedRepo _, _) -> Just $ RepoQR qualifiedRepo
    (_, Just (DependencyRegistryInfo _ (Just repo) _ _)) -> Just repo
    _ -> Nothing
