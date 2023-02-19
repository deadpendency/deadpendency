module DD.Serve.AppHandler
  ( appToHandler,
    CarrierStack,
  )
where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitGoogleLoggingC (AppEventEmitGoogleLoggingIOC (..))
import Common.Effect.EmitErrors.Carrier.EmitErrorsC
import Common.Effect.GitHub.AppSharedAuth.Carrier.AppSharedAuthGenerateC (AppSharedAuthGenerateC (..))
import Common.Effect.GitHub.FetchRepoFiles.Carrier.FetchRepoFilesGitHubC
import Common.Effect.GitHub.InstallationAuth.Carrier.InstallationAuthGitHubC (InstallationAuthGitHubIOC (..))
import Common.Effect.GitHub.SearchRepoDirectoryFiles.Carrier.SearchRepoDirectoryFilesGitHubC
import Common.Effect.GitHub.SearchRepoFiles.Carrier.SearchRepoFilesGitHubC
import Common.Effect.InterchangeEventDecode.Carrier.InterchangeEventDecodeC (InterchangeEventDecodeC (..))
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultC
import Common.Effect.PublishFailedMessage.Carrier.PublishFailedMessageC
import Common.Effect.QueueEventPublish.Carrier.QueueEventPublishGooglePubSubC (QueueEventPublishGooglePubSubIOC (..))
import Common.Effect.Trace.Carrier.TraceEmitGoogleC
import Common.Effect.TranslateError.Carrier.TranslateCatchC
import Common.Effect.TranslateError.Carrier.TranslateThrowC
import Common.Model.Config.CommonConfig (CommonConfig)
import Common.Model.Config.InstanceConfig (InstanceConfig)
import Common.Model.Details.ComponentDetails
import Common.Model.Details.ComponentTrace
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHAppAuthGlobal
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.InterchangeEvent.DependenciesDetermined
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunPrepared
import Common.Model.RepoConfig.RepoConfig
import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen (RandomC, evalRandomSystem)
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import DD.AppGoogleScopes (AppGoogleScopes)
import DD.Effect.DetermineDependencies.Carrier.DetermineDependenciesGitHubC
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import DD.Model.AppContext (AppContext (..))
import DD.Model.AppError (AppError)
import Network.Google.Env qualified as G
import Servant (Handler)
import Servant.Server (Handler (..), ServerError (..), err500)
import System.Random (StdGen)

{- ORMOLU_DISABLE -}
type CarrierStack =
  ( DetermineDependenciesGitHubIOC
  ( SearchRepoFilesGitHubIOC
  ( SearchRepoDirectoryFilesGitHubIOC
  ( FetchRepoFileGitHubIOC
  ( InstallationAuthGitHubIOC
  ( AppSharedAuthGenerateC
  ( ReaderC GHAppAuthGlobal
  ( PublishFailedMessageIOC RunPrepared
  ( PublishComponentResultIOC DependenciesDetermined
  ( QueueEventPublishGooglePubSubIOC AppGoogleScopes
  ( InterchangeEventLoadC RunPrepared
  ( InterchangeEventDecodeC (InterchangeEvent RunPrepared)
  ( TraceEmitGoogleIOC AppGoogleScopes
  ( RandomC StdGen
  ( TranslateCatchC DetermineDependenciesError AppError
  ( TranslateThrowC DetermineDependenciesError AppError
  ( TranslateThrowC CommonError AppError
  ( EmitErrorsC AppError
  ( AppEventEmitGoogleLoggingIOC AppGoogleScopes
  ( StateC (Maybe GHInstallationAuth)
  ( StateC (Maybe RunTrace)
  ( StateC (Maybe ComponentTrace)
  ( StateC (Maybe Run)
  ( StateC (Maybe CheckRun)
  ( StateC (Maybe RepoConfig)
  ( ReaderC (G.Env AppGoogleScopes)
  ( ReaderC ComponentDetails
  ( ReaderC InstanceConfig
  ( ReaderC CommonConfig IO
  )))))))))))))))))))))))))))))
{- ORMOLU_ENABLE -}

appToHandler ::
  AppContext ->
  CarrierStack a ->
  Handler a
appToHandler (AppContext googleEnv commonConfig instanceConfig componentDetails ghAuthGlobal) =
  runDetermineDependenciesGitHubIOC
    >>> runSearchRepoFilesGitHubIOC
    >>> runSearchRepoDirectoryFilesGitHubIOC
    >>> runFetchRepoFileGitHubIOC
    >>> runInstallationAuthGitHubIOC
    >>> runAppSharedAuthGenerateC
    >>> runReader @GHAppAuthGlobal ghAuthGlobal
    >>> runPublishFailedMessageIOC @RunPrepared
    >>> runPublishComponentResultIOC @DependenciesDetermined
    >>> runQueueEventPublishGooglePubSubIOC @AppGoogleScopes
    >>> runInterchangeEventLoadC @RunPrepared
    >>> runInterchangeEventDecodeC @(InterchangeEvent RunPrepared)
    >>> runTraceEmitGoogleIOC
    >>> evalRandomSystem
    >>> runTranslateCatchC @DetermineDependenciesError @AppError
    >>> runTranslateThrowC @DetermineDependenciesError @AppError
    >>> runTranslateThrowC @CommonError @AppError
    >>> runEmitErrorsC @AppError
    >>> runError @AppError
    >>> runAppEventEmitGoogleLoggingIOC @AppGoogleScopes
    >>> evalState @(Maybe GHInstallationAuth) Nothing
    >>> evalState @(Maybe RunTrace) Nothing
    >>> evalState @(Maybe ComponentTrace) Nothing
    >>> evalState @(Maybe Run) Nothing
    >>> evalState @(Maybe CheckRun) Nothing
    >>> evalState @(Maybe RepoConfig) Nothing
    >>> runReader @(G.Env AppGoogleScopes) googleEnv
    >>> runReader @ComponentDetails componentDetails
    >>> runReader @InstanceConfig instanceConfig
    >>> runReader @CommonConfig commonConfig
    >>> toHandler

toHandler :: IO (Either AppError a) -> Handler a
toHandler = Handler . ExceptT . toServerError

toServerError :: IO (Either AppError a) -> IO (Either ServerError a)
toServerError = firstF (const err500)
