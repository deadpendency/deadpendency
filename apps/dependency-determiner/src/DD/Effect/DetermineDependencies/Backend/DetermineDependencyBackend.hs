module DD.Effect.DetermineDependencies.Backend.DetermineDependencyBackend
  ( ignoreDeps,
    removeDuplicates,
    addAdditionalDeps,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Ignored.IgnoredDependency
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

ignoreDeps :: IgnoreDependenciesConfig -> NV.NonEmptyVector BasicDependency -> (IgnoredRepoDependencies, Maybe (NV.NonEmptyVector BasicDependency))
ignoreDeps ignoreDependenciesConfig basicRepoDeps
  | noIgnores ignoreDependenciesConfig = (IgnoredRepoDependencies V.empty, Just basicRepoDeps)
  | otherwise =
      let (ignoredDeps, filteredBasicRepoDeps) =
            NV.foldl' (foldBasicDeps ignoreDependenciesConfig) (V.empty, V.empty) basicRepoDeps
          resultBasicRepoDeps = NV.fromVector filteredBasicRepoDeps
          resultIgnoreDeps = IgnoredRepoDependencies ignoredDeps
       in (resultIgnoreDeps, resultBasicRepoDeps)

noIgnores :: IgnoreDependenciesConfig -> Bool
noIgnores (IDAll ignoreDepNames) = V.null ignoreDepNames
noIgnores (IDSpecific ignoreLanguageDeps) = V.null ignoreLanguageDeps

foldBasicDeps ::
  IgnoreDependenciesConfig ->
  (V.Vector IgnoredDependency, V.Vector BasicDependency) ->
  BasicDependency ->
  (V.Vector IgnoredDependency, V.Vector BasicDependency)
foldBasicDeps ignoreDependenciesConfig (ignored, basic) b@(BasicDependency pl dIdentifier@(DependencyIdentifierNamed dependencyName) maybeType)
  | namePlMatch ignoreDependenciesConfig pl dependencyName = (ignored `V.snoc` IgnoredDependency pl dIdentifier maybeType, basic)
  | otherwise = (ignored, basic `V.snoc` b)
foldBasicDeps ignoreDependenciesConfig (ignored, basic) b@(BasicDependency pl dIdentifier@(DependencyIdentifierRepo _ (Just dependencyName)) maybeType)
  | namePlMatch ignoreDependenciesConfig pl dependencyName = (ignored `V.snoc` IgnoredDependency pl dIdentifier maybeType, basic)
  | otherwise = (ignored, basic `V.snoc` b)
foldBasicDeps _ (ignored, basic) b = (ignored, basic `V.snoc` b)

namePlMatch :: IgnoreDependenciesConfig -> ProgrammingLanguage -> DependencyName -> Bool
namePlMatch (IDAll ignoreDepNames) _ dependencyName = V.elem dependencyName ignoreDepNames
namePlMatch (IDSpecific ignoreLanguageDeps) pl dependencyName =
  V.any
    ( \(IgnoreLanguageDependencies ildPl nvDeps) ->
        ildPl == pl
          && NV.elem dependencyName nvDeps
    )
    ignoreLanguageDeps

{- HLINT ignore "Eta reduce" -}
addAdditionalDeps :: V.Vector BasicDependency -> V.Vector BasicDependency -> V.Vector BasicDependency
addAdditionalDeps additionalDeps =
  foldl'
    ( \acc b ->
        if overridenByAddDep b acc
          then acc
          else acc `V.snoc` b
    )
    additionalDeps

overridenByAddDep :: BasicDependency -> V.Vector BasicDependency -> Bool
overridenByAddDep bd@(BasicDependency pl dependencyIdentifier _) =
  case dependencyIdentifier of
    DependencyIdentifierNamed dependencyName -> V.any (\d -> getDIName (d ^. #_dependencyIdentifier) == Just dependencyName && (plToRegistry (d ^. #_programmingLanguage) == plToRegistry pl))
    DependencyIdentifierRepo _ (Just dependencyName) -> V.any (\d -> getDIName (d ^. #_dependencyIdentifier) == Just dependencyName && (plToRegistry (d ^. #_programmingLanguage) == plToRegistry pl))
    DependencyIdentifierRepo _ Nothing -> anyContainMatching bd

removeDuplicates :: V.Vector BasicDependency -> V.Vector BasicDependency
removeDuplicates =
  foldl'
    ( \acc b ->
        if anyContainMatching b acc
          then acc
          else acc `V.snoc` b
    )
    V.empty

anyContainMatching :: BasicDependency -> V.Vector BasicDependency -> Bool
anyContainMatching (BasicDependency pl dependencyIdentifier _) =
  V.any (\d -> (d ^. #_dependencyIdentifier) == dependencyIdentifier && (plToRegistry (d ^. #_programmingLanguage) == plToRegistry pl))
