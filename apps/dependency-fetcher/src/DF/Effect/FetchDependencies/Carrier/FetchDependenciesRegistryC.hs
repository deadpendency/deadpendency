module DF.Effect.FetchDependencies.Carrier.FetchDependenciesRegistryC
  ( FetchDependenciesRegistryIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.CacheExternal.CacheExternal
import Common.Effect.Util
import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Dependency.Enriched.EnrichedRepoDependencies
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Dependency.Errored.ErroredRepoDependencies
import Common.Model.Ecosystem.ProgrammingLanguage
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw)
import DF.Effect.FetchDependencies.FetchDependencies
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchDependencies.Model.FetchDependenciesResult
import DF.Effect.FetchRegistryRepoInfo.FetchRegistryRepoInfo
import DF.Effect.FetchRepoStats.FetchRepoStats
import DF.Effect.FetchRepoStats.Model.DependencyFetchResult
import Data.HashMap.Strict qualified as HM
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

newtype FetchDependenciesRegistryIOC m a = FetchDependenciesRegistryIOC {runFetchDependenciesRegistryIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (CacheExternal EnrichedDependency) sig m,
    Has (Throw FetchDependenciesError) sig m,
    Has FetchRegistryRepoInfo sig m,
    Has FetchRepoStats sig m
  ) =>
  Algebra (FetchDependencies :+: sig) (FetchDependenciesRegistryIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (FetchDependencies request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Fetch dependencies") (AppEventAdditional request)
      let basicDependencies = request ^. (#_repositoryDependencies . #_dependencies)
      dependencyFetchResults <- enrichDependenciesWithCache basicDependencies
      let (failedDeps, enrichedDeps) = NV.partitionWith partitionResultEither dependencyFetchResults
          processingFailures = hasProcessingFailure failedDeps

      when
        (not (V.null processingFailures))
        (emitAppEvent $ AppEvent AppEventLevelError (AppEventMessage "During: Fetch dependencies - Processing Failures!") (Just $ AppEventAdditional processingFailures))

      let erroredRepoDependencies = ErroredRepoDependencies failedDeps
      nvEnrichedDeps <- maybeToErrorM (AllDepsFailedFetch $ NV.unsafeFromVector $ erroredRepoDependencies ^. #_dependencies) (NV.fromVector enrichedDeps)
      let result =
            FetchDependenciesResult
              { _enrichedRepoDependencies = EnrichedRepoDependencies nvEnrichedDeps,
                _erroredRepoDependencies = erroredRepoDependencies
              }

      emitAppEventInfoA (AppEventMessage "Finished: Fetch dependencies") (AppEventAdditional $ onlyFirst25Deps result)
      FetchDependenciesRegistryIOC $ pure (ctx $> result)
    (R other) -> FetchDependenciesRegistryIOC $ alg (runFetchDependenciesRegistryIOC . hdl) other ctx

-- too many deps will exceed the 256KB logging limit of stackdriver
onlyFirst25Deps :: FetchDependenciesResult -> FetchDependenciesResult
onlyFirst25Deps fdr =
  fdr
    & (#_enrichedRepoDependencies . #_dependencies) %~ (NV.unsafeFromVector . NV.take 25)

hasProcessingFailure :: V.Vector ErroredDependency -> V.Vector ErroredDependency
hasProcessingFailure =
  V.filter
    ( \e ->
        case e ^. #_erroredReason of
          UnexpectedFailureToParseRegistryEntry _ -> True
          UnexpectedFailureRegistryDataInconsistent _ -> True
          _ -> False
    )

bdToKey :: BasicDependency -> Maybe Text
bdToKey (BasicDependency pl di _) = plToRegistryKey pl <&> \plKey -> plKey <> "-" <> getDIKey di

plToRegistryKey :: ProgrammingLanguage -> Maybe Text
plToRegistryKey =
  \case
    JavaScript -> Just "npm"
    TypeScript -> Just "npm"
    Python -> Just "pypi"
    Php -> Just "packagist"
    Ruby -> Just "rubygems"
    Haskell -> Just "hackage"
    Rust -> Just "crates"
    CSharpNet -> Just "nuget"
    VisualBasicNet -> Just "nuget"
    Java -> Just "maven"
    Kotlin -> Just "maven"
    Scala -> Just "maven"
    Golang -> Just "pkggodev"
    UnsupportedLanguage _ -> Nothing

erdToKey :: EnrichedDependency -> Maybe Text
erdToKey (EnrichedDependency pl di _ _) = plToRegistryKey pl <&> \plKey -> plKey <> "-" <> getDIKey di

enrichDependenciesWithCache ::
  ( Has (CacheExternal EnrichedDependency) sig m,
    Has (Throw FetchDependenciesError) sig m,
    Has FetchRegistryRepoInfo sig m,
    Has FetchRepoStats sig m
  ) =>
  NV.NonEmptyVector BasicDependency ->
  m (NV.NonEmptyVector DependencyFetchResult)
enrichDependenciesWithCache deps = do
  -- load from cache
  let keys = NV.mapMaybe bdToKey deps
  loadedHashMap <-
    case NV.fromVector keys of
      Just nvKeys -> loadFromCache @EnrichedDependency nvKeys
      Nothing -> pure HM.empty

  let loadedKeys = V.fromList $ HM.keys loadedHashMap

      -- fetch unmatched deps from registry / repo
      unmatched = getUnmatchedDeps deps loadedKeys
  newlyFetchedFetchResults <-
    case NV.fromVector unmatched of
      Just nvUnmatched -> do
        -- get registry + repo info
        registryFetchResults <- fetchRegistryRepoInfo nvUnmatched
        let (erroredRegistryDeps, registryWithRepoDeps) = NV.partitionWith id registryFetchResults

        -- get repo stats
        results <-
          case NV.fromVector registryWithRepoDeps of
            Nothing -> pure V.empty
            Just nvRegistryWithRepoDeps -> NV.toVector <$> fetchRepoStats nvRegistryWithRepoDeps

        -- store fetched deps in cache
        let (_, enrichedDeps) = V.partitionWith partitionResultEither results
            enrichedKnownLanguage = V.filter isNotUnknownLanguage enrichedDeps
        when
          (not (V.null enrichedKnownLanguage))
          (storeInCache 600 $ HM.fromList $ V.toList $ removeMaybeKey $ fmapToFst erdToKey enrichedKnownLanguage)

        pure $ (DFRErrored <$> erroredRegistryDeps) <> results
      Nothing -> pure V.empty

  -- collate the combined list of cached + fetched
  let combindedDepsAsResults = newlyFetchedFetchResults <> (DFRSuccess <$> V.fromList (HM.elems loadedHashMap))
  maybeToErrorM UnexpectedEmptyAfterProcessing $ NV.fromVector combindedDepsAsResults

isNotUnknownLanguage :: EnrichedDependency -> Bool
isNotUnknownLanguage (EnrichedDependency (UnsupportedLanguage _) _ _ _) = False
isNotUnknownLanguage _ = True

removeMaybeKey :: V.Vector (Maybe Text, EnrichedDependency) -> V.Vector (Text, EnrichedDependency)
removeMaybeKey =
  V.foldl'
    ( \acc a ->
        case a of
          (Just key, erd) -> acc `V.snoc` (key, erd)
          (Nothing, _) -> error "Unexpected not all known languages"
    )
    V.empty

getUnmatchedDeps :: NV.NonEmptyVector BasicDependency -> V.Vector Text -> V.Vector BasicDependency
getUnmatchedDeps allDeps cachedKeys =
  NV.foldl'
    ( \unmatched bd ->
        case bdToKey bd of
          Nothing -> V.snoc unmatched bd
          Just bdKey ->
            if V.notElem bdKey cachedKeys
              then V.snoc unmatched bd
              else unmatched
    )
    V.empty
    allDeps
