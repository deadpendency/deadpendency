module RP.Serve.AppHandler
  ( appToHandler,
    CarrierStack,
  )
where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitGoogleLoggingC (AppEventEmitGoogleLoggingIOC (..))
import Common.Effect.CacheExternal.Carrier.CacheExternalRedisC
import Common.Effect.EmitErrors.Carrier.EmitErrorsC
import Common.Effect.GitHub.AppSharedAuth.Carrier.AppSharedAuthGenerateC (AppSharedAuthGenerateC (..))
import Common.Effect.GitHub.CountPrivateInstalls.Carrier.CountPrivateInstallsGitHubC
import Common.Effect.GitHub.DetermineAccountPlan.Carrier.DetermineAccountPlanGitHubC
import Common.Effect.GitHub.FetchRepoFiles.Carrier.FetchRepoFilesGitHubC
import Common.Effect.GitHub.InstallationAuth.Carrier.InstallationAuthGitHubC (InstallationAuthGitHubIOC (..))
import Common.Effect.InterchangeEventDecode.Carrier.InterchangeEventDecodeC (InterchangeEventDecodeC (..))
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultC
import Common.Effect.QueueEventPublish.Carrier.QueueEventPublishGooglePubSubC (QueueEventPublishGooglePubSubIOC (..))
import Common.Effect.Trace.Carrier.TraceEmitGoogleC
import Common.Effect.TranslateError.Carrier.TranslateCatchC
import Common.Effect.TranslateError.Carrier.TranslateThrowC
import Common.Model.Cache.CacheConfigGlobal
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
import Common.Model.InterchangeEvent.CheckRunCreated
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunPrepared
import Common.Model.RepoConfig.RepoConfig
import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen (RandomC, evalRandomSystem)
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Network.Google.Env qualified as G
import RP.AppGoogleScopes (AppGoogleScopes)
import RP.Effect.ReadConfig.Carrier.ReadConfigGitHubC
import RP.Effect.ReadConfig.Model.ReadConfigError
import RP.Effect.VerifyPlan.Carrier.VerifyPlanC
import RP.Model.AppContext (AppContext (..))
import RP.Model.AppError (AppError)
import Servant (Handler)
import Servant.Server (Handler (..), ServerError (..), err500)
import System.Random (StdGen)

{- ORMOLU_DISABLE -}
type CarrierStack =
  ( VerifyPlanIOC
  ( ReadConfigGitHubIOC
  ( DetermineAccountPlanGitHubIOC
  ( CountPrivateInstallsGitHubIOC
  ( CacheExternalRedisIOC ()
  ( FetchRepoFileGitHubIOC
  ( InstallationAuthGitHubIOC
  ( AppSharedAuthGenerateC
  ( ReaderC GHAppAuthGlobal
  ( PublishComponentResultIOC RunPrepared
  ( QueueEventPublishGooglePubSubIOC AppGoogleScopes
  ( InterchangeEventLoadC CheckRunCreated
  ( InterchangeEventDecodeC (InterchangeEvent CheckRunCreated)
  ( TraceEmitGoogleIOC AppGoogleScopes
  ( RandomC StdGen
  ( TranslateCatchC ReadConfigError AppError
  ( TranslateThrowC ReadConfigError AppError
  ( TranslateThrowC CommonError AppError
  ( EmitErrorsC AppError
  ( AppEventEmitGoogleLoggingIOC AppGoogleScopes
  ( StateC (Maybe GHInstallationAuth)
  ( StateC (Maybe RunTrace)
  ( StateC (Maybe ComponentTrace)
  ( StateC (Maybe Run)
  ( StateC (Maybe CheckRun)
  ( StateC (Maybe RepoConfig)
  ( ReaderC CacheConfigGlobal
  ( ReaderC (G.Env AppGoogleScopes)
  ( ReaderC ComponentDetails
  ( ReaderC InstanceConfig
  ( ReaderC CommonConfig IO
  )))))))))))))))))))))))))))))))
{- ORMOLU_ENABLE -}

appToHandler ::
  AppContext ->
  CarrierStack a ->
  Handler a
appToHandler (AppContext googleEnv commonConfig instanceConfig componentDetails ghAuthGlobal cacheConfigGlobal) =
  runVerifyPlanIOC
    >>> runReadConfigGitHubIOC
    >>> runDetermineAccountPlanGitHubIOC
    >>> runCountPrivateInstallsGitHubIOC
    >>> runCacheExternalRedisIOC
    >>> runFetchRepoFileGitHubIOC
    >>> runInstallationAuthGitHubIOC
    >>> runAppSharedAuthGenerateC
    >>> runReader @GHAppAuthGlobal ghAuthGlobal
    >>> runPublishComponentResultIOC @RunPrepared
    >>> runQueueEventPublishGooglePubSubIOC @AppGoogleScopes
    >>> runInterchangeEventLoadC @CheckRunCreated
    >>> runInterchangeEventDecodeC @(InterchangeEvent CheckRunCreated)
    >>> runTraceEmitGoogleIOC
    >>> evalRandomSystem
    >>> runTranslateCatchC @ReadConfigError @AppError
    >>> runTranslateThrowC @ReadConfigError @AppError
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
    >>> runReader @CacheConfigGlobal cacheConfigGlobal
    >>> runReader @(G.Env AppGoogleScopes) googleEnv
    >>> runReader @ComponentDetails componentDetails
    >>> runReader @InstanceConfig instanceConfig
    >>> runReader @CommonConfig commonConfig
    >>> toHandler

toHandler :: IO (Either AppError a) -> Handler a
toHandler = Handler . ExceptT . toServerError

toServerError :: IO (Either AppError a) -> IO (Either ServerError a)
toServerError = firstF (const err500)
