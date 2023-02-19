module SR.Serve.AppHandler
  ( appToHandler,
    CarrierStack,
  )
where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitGoogleLoggingC (AppEventEmitGoogleLoggingIOC (..))
import Common.Effect.EmitErrors.Carrier.EmitErrorsC
import Common.Effect.GitHub.AppSharedAuth.Carrier.AppSharedAuthGenerateC
import Common.Effect.GitHub.InstallationAuth.Carrier.InstallationAuthGitHubC
import Common.Effect.MetricEmit.Carrier.MetricEmitGoogleC
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
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Network.Google.Env qualified as G
import SR.AppGoogleScopes (AppGoogleScopes)
import SR.Effect.FetchInstallationsCount.Carrier.FetchInstallationsCountGitHubC
import SR.Effect.RunSmokeTest.Carrier.RunSmokeTestGitHubC
import SR.Effect.StreamQueueMessages.Carrier.StreamQueueMessagesGooglePubSubC
import SR.Model.AppContext (AppContext (..))
import SR.Model.AppError (AppError)
import SR.Model.Config
import Servant (Handler)
import Servant.Server (Handler (..), ServerError (..), err500)

{- ORMOLU_DISABLE -}
type CarrierStack =
  ( FetchInstallationsCountGitHubIOC
  ( RunSmokeTestGitHubIOC
  ( InstallationAuthGitHubIOC
  ( AppSharedAuthGenerateC
  ( ReaderC GHAppAuthGlobal
  ( MetricEmitGoogleIOC AppGoogleScopes
  ( StreamQueueMessagesGooglePubSubIOC AppGoogleScopes
  ( TranslateThrowC CommonError AppError
  ( EmitErrorsC AppError
  ( AppEventEmitGoogleLoggingIOC AppGoogleScopes
  ( StateC (Maybe GHInstallationAuth)
  ( StateC (Maybe RunTrace)
  ( StateC (Maybe ComponentTrace)
  ( StateC (Maybe Run)
  ( ReaderC (G.Env AppGoogleScopes)
  ( ReaderC ComponentDetails
  ( ReaderC InstanceConfig
  ( ReaderC Config
  ( ReaderC CommonConfig IO
  )))))))))))))))))))
{- ORMOLU_ENABLE -}

appToHandler ::
  AppContext ->
  CarrierStack a ->
  Handler a
appToHandler (AppContext googleEnv commonConfig instanceConfig componentDetails appConfig ghAuthGlobal) =
  runFetchInstallationsCountGitHubIOC
    >>> runRunSmokeTestGitHubIOC
    >>> runInstallationAuthGitHubIOC
    >>> runAppSharedAuthGenerateC
    >>> runReader @GHAppAuthGlobal ghAuthGlobal
    >>> runMetricEmitGoogleIOC
    >>> runStreamQueueMessagesGooglePubSubIOC
    >>> runTranslateThrowC @CommonError @AppError
    >>> runEmitErrorsC @AppError
    >>> runError @AppError
    >>> runAppEventEmitGoogleLoggingIOC @AppGoogleScopes
    >>> evalState @(Maybe GHInstallationAuth) Nothing
    >>> evalState @(Maybe RunTrace) Nothing
    >>> evalState @(Maybe ComponentTrace) Nothing
    >>> evalState @(Maybe Run) Nothing
    >>> runReader @(G.Env AppGoogleScopes) googleEnv
    >>> runReader @ComponentDetails componentDetails
    >>> runReader @InstanceConfig instanceConfig
    >>> runReader @Config appConfig
    >>> runReader @CommonConfig commonConfig
    >>> toHandler

toHandler :: IO (Either AppError a) -> Handler a
toHandler = Handler . ExceptT . toServerError

toServerError :: IO (Either AppError a) -> IO (Either ServerError a)
toServerError = firstF (const err500)
