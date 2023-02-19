{-# LANGUAGE DataKinds #-}

module Common.Effect.MetricEmit.Carrier.MetricEmitGoogleC
  ( MetricEmitGoogleIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.MetricEmit.MetricEmit (MetricEmit (..))
import Common.Effect.MetricEmit.Model.MetricEvent
import Common.Model.Config.AppEnv
import Common.Model.Config.CommonConfig
import Common.Model.Config.InstanceConfig
import Common.Model.Plan.Plan
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Data.HashMap.Strict qualified as HM
import GHC.TypeLits (Symbol)
import Network.Google qualified as GB
import Network.Google.Auth.Scope qualified as G
import Network.Google.Env qualified as G
import Network.Google.Monitoring.Types qualified as G
import Network.Google.Resource.Monitoring.Projects.TimeSeries.Create qualified as G

newtype MetricEmitGoogleIOC (s :: [Symbol]) m a = MetricEmitGoogleIOC {runMetricEmitGoogleIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has (Reader (G.Env s)) sig m,
    Has (Reader InstanceConfig) sig m,
    Has (Reader CommonConfig) sig m,
    Has AppEventEmit sig m,
    G.AllowScopes s,
    G.HasScope' s MonitoringRequiredScopes ~ 'True,
    MonadIO m
  ) =>
  Algebra (MetricEmit :+: sig) (MetricEmitGoogleIOC s m)
  where
  alg hdl sig ctx = case sig of
    L (MetricEmit event) -> do
      emitAppEventInfoA (AppEventMessage "Started: Publish Metric") (AppEventAdditional event)
      env <- ask @(G.Env s)
      commonConfig <- ask @CommonConfig
      currentTime <- liftIO getCurrentTime
      instanceConfig <- ask @InstanceConfig
      let appEnvText = appEnvAsText $ commonConfig ^. #_appEnv
          projectId = "projects/" <> instanceConfig ^. #_projectId
          createRequest =
            case event of
              AppInstalledMetricEvent plan accountName -> appInstalledResultRequest appEnvText currentTime plan accountName
              AppCancelledMetricEvent plan accountName -> appCancelledResultRequest appEnvText currentTime plan accountName
              SmokeTestResultEvent result -> smokeTestResultRequest appEnvText currentTime result
              InstallationsCountMetricEvent plan count -> installationsCountResultRequest appEnvText currentTime plan count

          request = G.projectsTimeSeriesCreate createRequest projectId

      emitAppEventInfo (AppEventMessage $ "During: Publish Metric - Metric Request: " <> show request)

      liftIO $ (GB.runResourceT . GB.runGoogle env . GB.send) request

      emitAppEventInfo (AppEventMessage "Finished: Publish Metric")
      MetricEmitGoogleIOC $ pure $ ctx $> ()
    R other -> MetricEmitGoogleIOC $ alg (runMetricEmitGoogleIOC . hdl) other ctx

type MonitoringRequiredScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform",
     "https://www.googleapis.com/auth/monitoring",
     "https://www.googleapis.com/auth/monitoring.write"
   ]

smokeTestResultRequest :: Text -> UTCTime -> Bool -> G.CreateTimeSeriesRequest
smokeTestResultRequest appEnvText currentTime result =
  G.createTimeSeriesRequest
    & G.ctsrTimeSeries
      .~ [ G.timeSeries
             & G.tsPoints
               .~ [ G.point
                      & G.pValue ?~ (G.typedValue & G.tvBoolValue ?~ result)
                      & G.pInterval ?~ (G.timeInterval & G.tiEndTime ?~ currentTime)
                  ]
             & G.tsMetric
               ?~ (G.metric & G.mType ?~ "custom.googleapis.com/deadpendency_action/" <> appEnvText <> "_smoke_success")
             & G.tsResource
               ?~ ( G.monitoredResource
                      & G.mrType ?~ "global"
                  )
         ]

installationsCountResultRequest :: Text -> UTCTime -> Plan -> Int -> G.CreateTimeSeriesRequest
installationsCountResultRequest appEnvText currentTime plan count =
  G.createTimeSeriesRequest
    & G.ctsrTimeSeries
      .~ [ G.timeSeries
             & G.tsPoints
               .~ [ G.point
                      & G.pValue ?~ (G.typedValue & G.tvInt64Value ?~ fromIntegral count)
                      & G.pInterval ?~ (G.timeInterval & G.tiEndTime ?~ currentTime)
                  ]
             & G.tsMetric
               ?~ ( G.metric
                      & G.mType ?~ "custom.googleapis.com/deadpendency_action/" <> appEnvText <> "_total_installations"
                      & G.mLabels ?~ G.metricLabels (HM.fromList [("plan_type", planText plan)])
                  )
             & G.tsResource
               ?~ ( G.monitoredResource
                      & G.mrType ?~ "global"
                  )
         ]

appInstalledResultRequest :: Text -> UTCTime -> Plan -> Text -> G.CreateTimeSeriesRequest
appInstalledResultRequest appEnvText currentTime plan accountName =
  G.createTimeSeriesRequest
    & G.ctsrTimeSeries
      .~ [ G.timeSeries
             & G.tsPoints
               .~ [ G.point
                      & G.pValue ?~ (G.typedValue & G.tvInt64Value ?~ fromIntegral (planToPlanId plan))
                      & G.pInterval ?~ (G.timeInterval & G.tiEndTime ?~ currentTime)
                  ]
             & G.tsMetric
               ?~ ( G.metric
                      & G.mType ?~ "custom.googleapis.com/deadpendency_action/" <> appEnvText <> "_app_installations"
                      & G.mLabels ?~ G.metricLabels (HM.fromList [("plan_type", planText plan), ("login", accountName)])
                  )
             & G.tsResource
               ?~ ( G.monitoredResource
                      & G.mrType ?~ "global"
                  )
         ]

appCancelledResultRequest :: Text -> UTCTime -> Plan -> Text -> G.CreateTimeSeriesRequest
appCancelledResultRequest appEnvText currentTime plan accountName =
  G.createTimeSeriesRequest
    & G.ctsrTimeSeries
      .~ [ G.timeSeries
             & G.tsPoints
               .~ [ G.point
                      & G.pValue ?~ (G.typedValue & G.tvInt64Value ?~ fromIntegral (planToPlanId plan))
                      & G.pInterval ?~ (G.timeInterval & G.tiEndTime ?~ currentTime)
                  ]
             & G.tsMetric
               ?~ ( G.metric
                      & G.mType ?~ "custom.googleapis.com/deadpendency_action/" <> appEnvText <> "_app_cancellations"
                      & G.mLabels ?~ G.metricLabels (HM.fromList [("plan_type", planText plan), ("login", accountName)])
                  )
             & G.tsResource
               ?~ ( G.monitoredResource
                      & G.mrType ?~ "global"
                  )
         ]
