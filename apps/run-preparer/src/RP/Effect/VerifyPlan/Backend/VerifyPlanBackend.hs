module RP.Effect.VerifyPlan.Backend.VerifyPlanBackend (validatePlan) where

import Common.Effect.CacheExternal.CacheExternal
import Common.Effect.GitHub.CountPrivateInstalls.CountPrivateInstalls
import Common.Effect.GitHub.DetermineAccountPlan.DetermineAccountPlan
import Common.Model.Config.AppEnv
import Common.Model.Plan.Plan
import Common.Model.Plan.PlanError
import Control.Algebra (Has)

validatePlan ::
  ( Has (CacheExternal ()) sig m,
    Has DetermineAccountPlan sig m,
    Has CountPrivateInstalls sig m
  ) =>
  Bool ->
  AppEnv ->
  Bool ->
  Text ->
  m (Maybe PlanError)
validatePlan isDeadpendencyRun appEnv isPrivate cacheKey = runMaybeT $ do
  -- abort if deadpendency as they use an old direct app install with no plan
  _ <-
    if isDeadpendencyRun
      then hoistMaybe Nothing
      else hoistMaybe $ Just ()

  -- abort if not prod
  _ <-
    case appEnv of
      Prod -> hoistMaybe $ Just ()
      _ -> hoistMaybe Nothing

  -- abort if private
  _ <-
    if isPrivate
      then hoistMaybe $ Just ()
      else hoistMaybe Nothing

  maybePlanOkCached <- loadFromCacheSingle @() cacheKey

  -- abort if we cached a success
  _ <-
    if isJust maybePlanOkCached
      then hoistMaybe Nothing
      else hoistMaybe $ Just ()

  installsCountResult <- privateInstallsCount
  determinePlanResult <- accountPlanDetermine
  let installsCount = installsCountResult ^. #_count
      plan = determinePlanResult ^. #_plan

  case getPlanLimit plan of
    Nothing ->
      storeInCacheSingle 604800 cacheKey () *> hoistMaybe Nothing
    Just planLimit ->
      if installsCount > planLimit
        then hoistMaybe $ Just $ ExceededPlan plan installsCount
        else do
          storeInCacheSingle 604800 cacheKey ()
          hoistMaybe Nothing
