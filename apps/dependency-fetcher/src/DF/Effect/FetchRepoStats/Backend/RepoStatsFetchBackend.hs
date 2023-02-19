{-# LANGUAGE DataKinds #-}

module DF.Effect.FetchRepoStats.Backend.RepoStatsFetchBackend
  ( getRepoStats,
    toDependencyFetchResult,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.GitHub.Model.GitHubError
import Common.GitHub.RepoStats.FetchRepoStats
import Common.GitHub.RepoStats.Model.RepoStatsRequest
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.Auth.GHInstallationAuth
import Control.Concurrent (threadDelay)
import Control.Effect.Throw
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchRepoStats.Model.DependencyFetchResult
import DF.Effect.Model.FetchRegistryWithRepo
import Data.HashMap.Strict qualified as HM

{-
If we hit too much github will sometimes trigger abuse detection. In this case we identify it, take the reply after seconds header and block until that time has passed.
-}
getRepoStats ::
  ( Has AppEventEmit sig m,
    Has (Throw FetchDependenciesError) sig m,
    MonadIO m
  ) =>
  GHInstallationAuth ->
  QualifiedRepo ->
  m (Maybe (QualifiedRepo, DependencyRepoStats))
getRepoStats ghAuth qualifiedRepo = do
  let repoStatsRequest =
        RepoStatsRequest
          { _qualifiedRepo = qualifiedRepo
          }
  eitherRepoStatsResult <- liftIO $ fetchRepoStats ghAuth repoStatsRequest
  case eitherRepoStatsResult of
    Left (GHEAbuseDetectedError retryAfterSeconds) -> do
      emitAppEvent $ AppEvent AppEventLevelWarning (AppEventMessage $ "Abuse detected. Pausing for" <> show retryAfterSeconds) Nothing
      liftIO $ threadDelay $ retryAfterSeconds * 1000000
      emitAppEvent $ AppEvent AppEventLevelWarning (AppEventMessage "Abuse wait over. Resuming") Nothing
      getRepoStats ghAuth qualifiedRepo
    _ -> do
      maybeRepoStatsResult <- liftEither $ first RepoStatsFetchError eitherRepoStatsResult
      pure $
        maybeRepoStatsResult ^. #_dependencyRepoStats <&> (qualifiedRepo,)

toDependencyFetchResult :: HM.HashMap Text DependencyRepoStats -> FetchRegistryWithRepo -> DependencyFetchResult
toDependencyFetchResult hmRepoStats fetch =
  let bd = fetch ^. #_basicDependency
      dependencyIdentifier = bd ^. #_dependencyIdentifier
      programmingLanguage = bd ^. #_programmingLanguage
      dependencyType = bd ^. #_dependencyType
      theseWithRepo = fetch ^. #_results
      maybeRegistryInfo = theseWithRepo ^? here
      maybeRepo = theseWithRepo ^? (there . _Ctor @"RepoQR")
      maybeRepoStats = maybeRepo >>= \repo -> HM.lookup (getQRKey repo) hmRepoStats
      eitherTheseResult = theseFromMaybe NoRegistryOrRepoData maybeRegistryInfo maybeRepoStats
   in either
        (DFRErrored . ErroredDependency dependencyIdentifier dependencyType programmingLanguage Nothing)
        (DFRSuccess . EnrichedDependency programmingLanguage dependencyIdentifier dependencyType)
        eitherTheseResult
