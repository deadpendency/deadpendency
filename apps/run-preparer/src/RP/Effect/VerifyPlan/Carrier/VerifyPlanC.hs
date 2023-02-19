module RP.Effect.VerifyPlan.Carrier.VerifyPlanC
  ( VerifyPlanIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.CacheExternal.CacheExternal
import Common.Effect.GitHub.CountPrivateInstalls.CountPrivateInstalls
import Common.Effect.GitHub.DetermineAccountPlan.DetermineAccountPlan
import Common.Effect.Util
import Common.Model.Config.CommonConfig
import Common.Model.Details.Run
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Control.Effect.State (State)
import Control.Effect.Throw (Throw)
import RP.Effect.VerifyPlan.Backend.VerifyPlanBackend
import RP.Effect.VerifyPlan.VerifyPlan (VerifyPlan (..))

newtype VerifyPlanIOC m a = VerifyPlanIOC {runVerifyPlanIOC :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Throw CommonError) sig m,
    Has (State (Maybe Run)) sig m,
    Has (Reader CommonConfig) sig m,
    Has (CacheExternal ()) sig m,
    Has CountPrivateInstalls sig m,
    Has DetermineAccountPlan sig m
  ) =>
  Algebra (VerifyPlan :+: sig) (VerifyPlanIOC m)
  where
  alg hdl sig ctx = case sig of
    (L PlanVerify) -> do
      emitAppEventInfo (AppEventMessage "Started: Verify Plan")

      commonConfig <- ask @CommonConfig
      run <- getRun

      let isDeadpendencyRun = run ^. #_isDeadpendencyRun
          ownerAccountId = run ^. #_repoOwnerAccountId
          isPrivate = run ^. #_repoPrivate
          cacheKey = "valid-plan-" <> show ownerAccountId
          appEnv = commonConfig ^. #_appEnv

      maybePlanError <- validatePlan isDeadpendencyRun appEnv isPrivate cacheKey

      _ <-
        -- log if it fails
        case maybePlanError of
          Just planError -> emitAppEvent $ AppEvent AppEventLevelWarning (AppEventMessage "Plan Exceeded") (Just $ AppEventAdditional planError)
          Nothing -> pure ()

      emitAppEventInfoA (AppEventMessage "Finished: Verify Plan") (AppEventAdditional maybePlanError)

      -- if plan error then throw it and it gets converted to processing failure

      VerifyPlanIOC $ pure (ctx $> ())
    (R other) -> VerifyPlanIOC $ alg (runVerifyPlanIOC . hdl) other ctx
