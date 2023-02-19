module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Internal
  ( uniqueByName,
    anyContainMatchingName,
    isNotMatchedDep,
    dependencyIdentifierToName,
    dependencyIdentifierToQualifiedRepo,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Git.QualifiedRepo
import Data.Vector qualified as V

uniqueByName :: V.Vector BasicDependency -> V.Vector BasicDependency
uniqueByName =
  foldl'
    ( \acc b ->
        if anyContainMatchingName b acc
          then acc
          else acc `V.snoc` b
    )
    V.empty

anyContainMatchingName :: BasicDependency -> V.Vector BasicDependency -> Bool
anyContainMatchingName (BasicDependency _ dependencyIdentifier _) =
  V.any ((==) dependencyIdentifier . _dependencyIdentifier)

isNotMatchedDep :: Text -> BasicDependency -> Bool
isNotMatchedDep toMatch (BasicDependency _ dependencyIdentifier _) =
  case getDIName dependencyIdentifier of
    Just (DependencyName name) -> toMatch /= name
    Nothing -> False

dependencyIdentifierToName :: DependencyIdentifier -> Maybe Text
dependencyIdentifierToName di = getDIName di <&> \n -> n ^. #_ntText

dependencyIdentifierToQualifiedRepo :: DependencyIdentifier -> Maybe QualifiedRepo
dependencyIdentifierToQualifiedRepo =
  \case
    DependencyIdentifierRepo qualifiedRepo _ -> Just qualifiedRepo
    _ -> Nothing
