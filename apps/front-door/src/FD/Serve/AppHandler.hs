module FD.Serve.AppHandler
  ( appToHandler,
    CarrierStack,
  )
where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitGoogleLoggingC (AppEventEmitGoogleLoggingIOC (..))
import Common.Effect.EmitErrors.Carrier.EmitErrorsC
import Common.Effect.MetricEmit.Carrier.MetricEmitGoogleC
import Common.Effect.PublishSimpleResult.Carrier.PublishSimpleResultC
import Common.Effect.QueueEventPublish.Carrier.QueueEventPublishGooglePubSubC
import Common.Effect.Trace.Carrier.TraceEmitGoogleC
import Common.Effect.TranslateError.Carrier.TranslateThrowC
import Common.Model.Config.CommonConfig (CommonConfig)
import Common.Model.Config.InstanceConfig (InstanceConfig)
import Common.Model.Details.ComponentDetails
import Common.Model.Details.ComponentTrace
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Error.CommonError
import Common.Model.InterchangeEvent.RunCreated
import Control.Carrier.Error.Either
import Control.Carrier.Random.Gen (RandomC, evalRandomSystem)
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import FD.AppGoogleScopes (AppGoogleScopes)
import FD.Model.AppContext (AppContext (..))
import FD.Model.AppError (AppError)
import Network.Google.Env qualified as G
import Servant (Handler)
import Servant.Server (Handler (..), ServerError (..), err500)
import System.Random (StdGen)

{- ORMOLU_DISABLE -}
type CarrierStack =
  ( MetricEmitGoogleIOC AppGoogleScopes
  ( PublishSimpleResultIOC RunCreated
  ( QueueEventPublishGooglePubSubIOC AppGoogleScopes
  ( TraceEmitGoogleIOC AppGoogleScopes
  ( RandomC StdGen
  ( TranslateThrowC CommonError AppError
  ( EmitErrorsC AppError
  ( AppEventEmitGoogleLoggingIOC AppGoogleScopes
  ( ReaderC (G.Env AppGoogleScopes)
  ( StateC (Maybe RunTrace)
  ( StateC (Maybe ComponentTrace)
  ( StateC (Maybe Run)
  ( ReaderC ComponentDetails
  ( ReaderC InstanceConfig
  ( ReaderC CommonConfig IO
  )))))))))))))))
{- ORMOLU_ENABLE -}

appToHandler ::
  AppContext ->
  CarrierStack a ->
  Handler a
appToHandler (AppContext googleEnv _ commonConfig instanceConfig componentDetails _) =
  runMetricEmitGoogleIOC @AppGoogleScopes
    >>> runPublishSimpleResultIOC @RunCreated
    >>> runQueueEventPublishGooglePubSubIOC @AppGoogleScopes
    >>> runTraceEmitGoogleIOC
    >>> evalRandomSystem
    >>> runTranslateThrowC @CommonError @AppError
    >>> runEmitErrorsC @AppError
    >>> runError @AppError
    >>> runAppEventEmitGoogleLoggingIOC @AppGoogleScopes
    >>> runReader @(G.Env AppGoogleScopes) googleEnv
    >>> evalState @(Maybe RunTrace) Nothing
    >>> evalState @(Maybe ComponentTrace) Nothing
    >>> evalState @(Maybe Run) Nothing
    >>> runReader @ComponentDetails componentDetails
    >>> runReader @InstanceConfig instanceConfig
    >>> runReader @CommonConfig commonConfig
    >>> toHandler

toHandler :: IO (Either AppError a) -> Handler a
toHandler = Handler . ExceptT . toServerError

toServerError :: IO (Either AppError a) -> IO (Either ServerError a)
toServerError = firstF (const err500)
