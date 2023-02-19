{-# LANGUAGE BangPatterns #-}

module DF.Effect.FetchRegistryRepoInfo.Carrier.FetchRegistryRepoInfoC
  ( FetchRegistryRepoInfoIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.CacheExternal.CacheExternal
import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.Enriched.EnrichedDependency
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchRegistryRepoInfo.Backend.RegistryFetchOrchestrateBackend
import DF.Effect.FetchRegistryRepoInfo.FetchRegistryRepoInfo
import DF.Effect.FetchRegistryRepoInfo.Model.FetchRegistryRepoInfoResult
import Data.Vector.NonEmpty qualified as NV
import Streamly.Prelude qualified as S

newtype FetchRegistryRepoInfoIOC m a = FetchRegistryRepoInfoIOC {runFetchRegistryRepoInfoIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (CacheExternal EnrichedDependency) sig m,
    Has (Throw FetchDependenciesError) sig m,
    MonadIO m
  ) =>
  Algebra (FetchRegistryRepoInfo :+: sig) (FetchRegistryRepoInfoIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (FetchRegistryRepoInfo basicDependencies)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Fetch registry deps") (AppEventAdditional $ NV.take 50 basicDependencies)
      eitherDependencyFetchResults <- liftIO $ getAllRegistryInfo basicDependencies
      dependencyFetchResults <- liftEither eitherDependencyFetchResults

      emitAppEventInfoA (AppEventMessage "Finished: Fetch registry deps") (AppEventAdditional $ NV.take 25 dependencyFetchResults)
      FetchRegistryRepoInfoIOC $ pure (ctx $> dependencyFetchResults)
    (R other) -> FetchRegistryRepoInfoIOC $ alg (runFetchRegistryRepoInfoIOC . hdl) other ctx

getAllRegistryInfo ::
  NV.NonEmptyVector BasicDependency ->
  IO (Either FetchDependenciesError (NV.NonEmptyVector FetchRegistryRepoInfoResult))
getAllRegistryInfo input = do
  result <- fmap sequenceA $ S.foldl' (\acc !ri -> ri : acc) [] $ S.maxThreads 3 $ S.fromAhead $ S.mapM getRegistryInfo $ S.fromFoldable input
  pure $
    result
      >>= \deps ->
        case NV.fromList deps of
          Just nevDeps -> Right nevDeps
          _ -> Left UnexpectedEmptyDependenciesInStream
