{-# LANGUAGE DataKinds #-}

module Common.Effect.AppEventEmit.Carrier.AppEventEmitGoogleLoggingC
  ( AppEventEmitGoogleLoggingIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit (AppEventEmit (..))
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventLevel (AppEventLevel (..))
import Common.Effect.AppEventEmit.Model.AppEventPayload
import Common.Model.Config.AppEnv
import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Details.ComponentDetails
import Common.Model.Details.ComponentTrace
import Common.Model.Details.Run
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Control.Effect.State (State, get)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap (toHashMapText)
import GHC.TypeLits (Symbol)
import Network.Google qualified as GB
import Network.Google.Auth.Scope qualified as G
import Network.Google.Env qualified as G
import Network.Google.Logging.Types qualified as G
import Network.Google.Resource.Logging.Entries.Write qualified as G

newtype AppEventEmitGoogleLoggingIOC (s :: [Symbol]) m a = AppEventEmitGoogleLoggingIOC {runAppEventEmitGoogleLoggingIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has (Reader (G.Env s)) sig m,
    Has (Reader CommonConfig) sig m,
    Has (Reader InstanceConfig) sig m,
    Has (Reader ComponentDetails) sig m,
    Has (State (Maybe ComponentTrace)) sig m,
    Has (State (Maybe Run)) sig m,
    G.AllowScopes s,
    G.HasScope' s LogRequiredScopes ~ 'True,
    MonadIO m
  ) =>
  Algebra (AppEventEmit :+: sig) (AppEventEmitGoogleLoggingIOC s m)
  where
  alg hdl sig ctx = case sig of
    L (EmitAppEvent appEvent) -> do
      env <- ask @(G.Env s)
      commonConfig <- ask @CommonConfig
      instanceConfig <- ask @InstanceConfig
      componentDetails <- ask @ComponentDetails
      maybeComponentTrace <- (get @(Maybe ComponentTrace))
      maybeRun <- get @(Maybe Run)
      let appEventPayload = createAppEventPayload componentDetails maybeRun appEvent
          writeRequest = produceWriteLogRequest instanceConfig commonConfig componentDetails maybeComponentTrace appEvent appEventPayload
          innerLogic = (GB.runResourceT . GB.runGoogle env . GB.send . G.entriesWrite) writeRequest
      ctx <$ liftIO innerLogic
    R other -> AppEventEmitGoogleLoggingIOC $ alg (runAppEventEmitGoogleLoggingIOC . hdl) other ctx

type LogRequiredScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform",
     "https://www.googleapis.com/auth/logging.admin",
     "https://www.googleapis.com/auth/logging.write"
   ]

createAppEventPayload :: ComponentDetails -> Maybe Run -> AppEvent -> AppEventPayload
createAppEventPayload componentDetails run appEvent =
  let message = appEvent ^. #_message
      additional = appEvent ^. #_additional
   in AppEventPayload
        { _appEventMessage = message,
          _componentDetails = componentDetails,
          _run = run,
          _appEventAdditional = additional
        }

produceWriteLogRequest ::
  InstanceConfig ->
  CommonConfig ->
  ComponentDetails ->
  Maybe ComponentTrace ->
  AppEvent ->
  AppEventPayload ->
  G.WriteLogEntriesRequest
produceWriteLogRequest instanceConfig commonConfig componentDetails maybeComponentTrace appEvent appEventPayload =
  let componentTextName = componentDetails ^. #_componentTextName
      logName = "projects/dgtw-deadpendency-action-2/logs/" <> componentTextName
      configurationName = commonConfig ^. #_cloudRunConfiguration
      serviceName = commonConfig ^. #_cloudRunServiceName
      revisionName = commonConfig ^. #_cloudRunRevision
      version = commonConfig ^. (#_appVersion . #_ntText)
      appEnv = appEnvAsText $ commonConfig ^. #_appEnv
      region = instanceConfig ^. #_instanceRegion
      projectId = instanceConfig ^. #_projectId
      severity = getSeverity (appEvent ^. #_level)
      maybeComponentTraceId = maybeComponentTrace <&> \c -> "projects/dgtw-deadpendency-action-2/traces/" <> c ^. #_traceId
      maybeComponentSpanId = maybeComponentTrace <&> \c -> c ^. #_spanId
      jsonPayload = unwrapValue $ toJSON appEventPayload
      entry =
        G.logEntry
          & G.leJSONPayload ?~ G.logEntryJSONPayload jsonPayload
          & G.leTrace .~ maybeComponentTraceId
          & G.leSpanId .~ maybeComponentSpanId
          & G.leSeverity ?~ severity
      entries = [entry]
      resourceLabels =
        -- https://cloud.google.com/run/docs/logging#log-resource

        -- https://cloud.google.com/run/docs/reference/container-contract#env-vars
        G.monitoredResourceLabels $
          fromList
            [ ("configuration_name", configurationName),
              ("location", region),
              ("project_id", projectId),
              ("service_name", serviceName),
              ("revision_name", revisionName)
            ]

      labels =
        G.writeLogEntriesRequestLabels $
          fromList
            [ ("version", version),
              ("appEnv", appEnv)
            ]

      resource =
        G.monitoredResource
          & G.mrType ?~ "cloud_run_revision"
          & G.mrLabels ?~ resourceLabels
   in G.writeLogEntriesRequest
        & G.wlerEntries .~ entries
        & G.wlerLogName ?~ logName
        & G.wlerResource ?~ resource
        & G.wlerLabels ?~ labels

-- & G.wlerLabels ?~ labels

-- we avoid using proper CommonError for this so we don't get a loop of logging emitting errors, which need to be logged and so on
unwrapValue :: Value -> HashMap Text Value
unwrapValue (Object x) = toHashMapText x
unwrapValue _ = error "Unexpected payload is not JSON object"

getSeverity :: AppEventLevel -> G.LogEntrySeverity
getSeverity AppEventLevelDebug = G.Debug
getSeverity AppEventLevelInfo = G.Info
getSeverity AppEventLevelWarning = G.Warning
getSeverity AppEventLevelError = G.Error'
