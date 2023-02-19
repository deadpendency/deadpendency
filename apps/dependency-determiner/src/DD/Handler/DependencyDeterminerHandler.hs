{-# LANGUAGE DataKinds #-}

module DD.Handler.DependencyDeterminerHandler
  ( dependencyDeterminerHandler,
    DependencyDeterminerRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad (InterchangeEventLoad (..), loadInterchangeEvent)
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult
import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Common.Effect.PublishFailedMessage.PublishFailedMessage
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.Dependency.Basic.BasicRepoDependencies
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Common.Model.InterchangeEvent.DependenciesDetermined
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunPrepared
import Control.Algebra (Has)
import Control.Effect.Catch (Catch)
import DD.Effect.DetermineDependencies.DetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesRequest
import Network.Google.PubSub qualified as G
import Servant.API

type DependencyDeterminerRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

dependencyDeterminerHandler ::
  ( Has AppEventEmit sig m,
    Has TraceEmit sig m,
    Has DetermineDependencies sig m,
    Has (Catch DetermineDependenciesError) sig m,
    Has (PublishComponentResult DependenciesDetermined) sig m,
    Has (InterchangeEventLoad RunPrepared) sig m,
    Has (PublishFailedMessage RunPrepared) sig m
  ) =>
  Text ->
  G.ReceivedMessage ->
  m NoContent
dependencyDeterminerHandler googleTraceId receivedMessage = do
  (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Dependency Determiner")

  emitAppEventInfo $ AppEventMessage "Receive raw message"

  -- decode queue event
  interchangeEvent <- loadInterchangeEvent @RunPrepared receivedMessage

  emitAppEventInfoA (AppEventMessage "Receive dependency determiner") (AppEventAdditional interchangeEvent)

  -- do the work
  let mDependencyResult = determineDependencies (buildDetermineDependenciesRequest interchangeEvent)

  componentResult <-
    handleErrorWithAdditional @DetermineDependenciesError
      mDependencyResult
      ( \dependencyResult -> do
          let repoDependencies = dependencyResult ^. #_basicRepoDependencies
              ignoredDependencies = dependencyResult ^. #_ignoredRepoDependencies
              ddInterchangeEvent = buildDependenciesDetermined ignoredDependencies repoDependencies

          pure $ SuccessComponentResult ddInterchangeEvent
      )
      -- parse failures should result in a failed message (for now at least)
      -- it is much more likely a parse failure is a deadpendency bug, rather than a bad file
      ( \case
          UnableToParseDependencyFile {} -> publishFailedMessage $ FailedInterchangeEvent interchangeEvent
          _ -> pure ()
      )

  -- finish up
  publishComponentResult componentResult
  concludeSpan (ConcludeSpanRequest span)
  pure NoContent

buildDetermineDependenciesRequest :: InterchangeEvent RunPrepared -> DetermineDependenciesRequest
buildDetermineDependenciesRequest interchangeEvent =
  let runPrepared = interchangeEvent ^. #_result
      programmingLanguages = runPrepared ^. #_repoProgrammingLanguages
      run = interchangeEvent ^. #_run
      repoConfig = interchangeEvent ^. #_repoConfig
      qualifiedRepo = run ^. #_qualifiedRepo
      commitSha = run ^. #_gitHeadSha
      additionalDeps = repoConfig ^. #_additionalDependencies
      ignoredLanguageDeps = repoConfig ^. #_ignoreDependenciesConfig
      additionalDependencyFiles = repoConfig ^. #_additionalDependencyFiles
   in DetermineDependenciesRequest
        { _programmingLanguages = programmingLanguages,
          _qualifiedRepo = qualifiedRepo,
          _commitSha = commitSha,
          _additionalDependencies = additionalDeps,
          _ignoreDependenciesConfig = ignoredLanguageDeps,
          _additionalDependencyFiles = additionalDependencyFiles
        }

buildDependenciesDetermined :: IgnoredRepoDependencies -> BasicRepoDependencies -> DependenciesDetermined
buildDependenciesDetermined ignoredRepoDependencies basicRepoDependencies =
  DependenciesDetermined
    { _basicRepoDependencies = basicRepoDependencies,
      _ignoredRepoDependencies = ignoredRepoDependencies
    }
