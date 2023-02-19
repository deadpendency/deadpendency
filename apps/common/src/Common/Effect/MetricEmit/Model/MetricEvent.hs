module Common.Effect.MetricEmit.Model.MetricEvent
  ( MetricEvent (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Plan.Plan
import Data.Aeson

data MetricEvent
  = AppInstalledMetricEvent Plan Text
  | AppCancelledMetricEvent Plan Text
  | SmokeTestResultEvent Bool
  | InstallationsCountMetricEvent Plan Int
  deriving stock (Eq, Show, Generic)

instance ToJSON MetricEvent where
  toJSON = genericToJSON cleanJSONOptions
