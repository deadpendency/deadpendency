{-# LANGUAGE DataKinds #-}

module SR.Handler.EmitTotalInstallsMetricHandler
  ( emitTotalInstallsMetricHandler,
    EmitTotalInstallsRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.MetricEmit.MetricEmit
import Common.Effect.MetricEmit.Model.MetricEvent
import Common.Model.Plan.Plan
import Control.Algebra (Has)
import SR.Effect.FetchInstallationsCount.FetchInstallationsCount
import Servant (JSON, NoContent (..), Post, (:>))

type EmitTotalInstallsRoute =
  "emit-total-installs"
    :> Post '[JSON] NoContent

emitTotalInstallsMetricHandler ::
  ( Has AppEventEmit sig m,
    Has FetchInstallationsCount sig m,
    Has MetricEmit sig m
  ) =>
  m NoContent
emitTotalInstallsMetricHandler = do
  emitAppEventInfo (AppEventMessage "Started Emit Total Installs")

  betaPlanDeveloperCount <- installationsCountFetch BetaPlan
  individualDeveloperCount <- installationsCountFetch IndividualDeveloperPlan
  openSourceCount <- installationsCountFetch OpenSourceOrganizationPlan
  smallCount <- installationsCountFetch SmallPlan
  mediumCount <- installationsCountFetch MediumPlan
  largeCount <- installationsCountFetch LargePlan

  metricEmit $ InstallationsCountMetricEvent BetaPlan betaPlanDeveloperCount
  metricEmit $ InstallationsCountMetricEvent IndividualDeveloperPlan individualDeveloperCount
  metricEmit $ InstallationsCountMetricEvent OpenSourceOrganizationPlan openSourceCount
  metricEmit $ InstallationsCountMetricEvent SmallPlan smallCount
  metricEmit $ InstallationsCountMetricEvent MediumPlan mediumCount
  metricEmit $ InstallationsCountMetricEvent LargePlan largeCount

  emitAppEventInfo (AppEventMessage "Finished Emit Total Installs")
  pure NoContent
