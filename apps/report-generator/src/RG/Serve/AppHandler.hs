module RG.Serve.AppHandler
  ( appToHandler,
    CarrierStack,
  )
where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitGoogleLoggingC (AppEventEmitGoogleLoggingIOC (..))
import Common.Effect.EmitErrors.Carrier.EmitErrorsC
import Common.Effect.InterchangeEventDecode.Carrier.InterchangeEventDecodeC (InterchangeEventDecodeC (..))
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultC
import Common.Effect.QueueEventPublish.Carrier.QueueEventPublishGooglePubSubC (QueueEventPublishGooglePubSubIOC (..))
import Common.Effect.Trace.Carrier.TraceEmitGoogleC
import Common.Effect.TranslateError.Carrier.TranslateThrowC
import Common.Model.Config.CommonConfig (CommonConfig)
import Common.Model.Config.InstanceConfig (InstanceConfig)
import Common.Model.Details.ComponentDetails
import Common.Model.Details.ComponentTrace
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Error.CommonError
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRun
import Common.Model.InterchangeEvent.DependenciesFetched
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunResult
import Common.Model.RepoConfig.RepoConfig
import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen (RandomC, evalRandomSystem)
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Network.Google.Env qualified as G
import RG.AppGoogleScopes (AppGoogleScopes)
import RG.Effect.AssessDependencies.Carrier.AssessDependenciesC
import RG.Effect.GenerateReport.Carrier.GenerateReportC
import RG.Model.AppContext (AppContext (..))
import RG.Model.AppError (AppError)
import Servant (Handler)
import Servant.Server (Handler (..), ServerError (..), err500)
import System.Random (StdGen)

{- ORMOLU_DISABLE -}
type CarrierStack =
  ( GenerateReportC
  ( AssessDependenciesC
  ( PublishComponentResultIOC RunResult
  ( QueueEventPublishGooglePubSubIOC AppGoogleScopes
  ( InterchangeEventLoadC DependenciesFetched
  ( InterchangeEventDecodeC (InterchangeEvent DependenciesFetched)
  ( TraceEmitGoogleIOC AppGoogleScopes
  ( RandomC StdGen
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
  )))))))))))))))))))))
{- ORMOLU_ENABLE -}

appToHandler ::
  AppContext ->
  CarrierStack a ->
  Handler a
appToHandler (AppContext googleEnv commonConfig instanceConfig componentDetails) =
  runGenerateReportC
    >>> runAssessDependenciesC
    >>> runPublishComponentResultIOC @RunResult
    >>> runQueueEventPublishGooglePubSubIOC @AppGoogleScopes
    >>> runInterchangeEventLoadC @DependenciesFetched
    >>> runInterchangeEventDecodeC @(InterchangeEvent DependenciesFetched)
    >>> runTraceEmitGoogleIOC
    >>> evalRandomSystem
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
