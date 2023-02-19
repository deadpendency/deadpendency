{-# LANGUAGE DataKinds #-}

module DF.Effect.FetchRepoStats.Carrier.FetchRepoStatsGitHubC
  ( FetchRepoStatsGithubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.CacheExternal.CacheExternal
import Common.Effect.GitHub.InstallationAuth.InstallationAuth
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.Auth.GHInstallationAuth
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw)
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchRepoStats.Backend.RepoStatsFetchBackend
import DF.Effect.FetchRepoStats.FetchRepoStats
import DF.Effect.FetchRepoStats.Model.DependencyFetchResult
import DF.Effect.Model.FetchRegistryWithRepo
import Data.HashMap.Strict qualified as HM
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

newtype FetchRepoStatsGithubIOC m a = FetchRepoStatsGithubIOC {runFetchRepoStatsGithubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (CacheExternal EnrichedDependency) sig m,
    Has (Throw FetchDependenciesError) sig m,
    Has InstallationAuth sig m,
    MonadIO m
  ) =>
  Algebra (FetchRepoStats :+: sig) (FetchRepoStatsGithubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (FetchRepoStats fetchedDepWithRepos)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Fetch Repo Stats") (AppEventAdditional $ NV.take 25 fetchedDepWithRepos)
      ghAuth <- existingInstallationAuth
      dependencyFetchResults <- getAllRepoStats ghAuth fetchedDepWithRepos

      emitAppEventInfoA (AppEventMessage "Finished: Fetch Repo Stats") (AppEventAdditional $ NV.take 10 dependencyFetchResults)
      FetchRepoStatsGithubIOC $ pure (ctx $> dependencyFetchResults)
    (R other) -> FetchRepoStatsGithubIOC $ alg (runFetchRepoStatsGithubIOC . hdl) other ctx

getAllRepoStats ::
  ( Has AppEventEmit sig m,
    Has (Throw FetchDependenciesError) sig m,
    MonadIO m
  ) =>
  GHInstallationAuth ->
  NV.NonEmptyVector FetchRegistryWithRepo ->
  m (NV.NonEmptyVector DependencyFetchResult)
getAllRepoStats ghAuth fetches = do
  let repos = ordNubV $ concatMaybeNV $ fetches <&> (\fr -> fr ^? #_results . there . _Ctor @"RepoQR")
  result <- traverse (getRepoStats ghAuth) repos
  let hashMapResults = HM.fromList $ firstF getQRKey $ V.toList $ concatMaybeV result
      nvDepResults = fetches <&> toDependencyFetchResult hashMapResults

  pure nvDepResults
