module Common.Effect.GitHub.DetermineAccountPlan.Carrier.DetermineAccountPlanGitHubC
  ( DetermineAccountPlanGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.AppSharedAuth.AppSharedAuth
import Common.Effect.GitHub.DetermineAccountPlan.Backend.DetermineAccountPlanBackend
import Common.Effect.GitHub.DetermineAccountPlan.DetermineAccountPlan (DetermineAccountPlan (..))
import Common.Effect.GitHub.DetermineAccountPlan.Model.DetermineAccountPlanResult
import Common.Effect.Util
import Common.Model.Details.Run
import Common.Model.Error.CommonError
import Common.Model.Plan.Plan
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.State (State)
import Control.Effect.Throw (Throw, liftEither)

newtype DetermineAccountPlanGitHubIOC m a = DetermineAccountPlanGitHubIOC {runDetermineAccountPlanGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has AppSharedAuth sig m,
    Has (State (Maybe Run)) sig m,
    Has (Throw CommonError) sig m
  ) =>
  Algebra (DetermineAccountPlan :+: sig) (DetermineAccountPlanGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L AccountPlanDetermine) -> do
      emitAppEventInfo (AppEventMessage "Started: Determine Account Plan")
      sharedAuth <- obtainAppSharedAuth
      run <- getRun
      eitherMaybeMaketplaceResult <- liftIO $ githubDetermineAccountPlan sharedAuth (run ^. #_repoOwnerAccountId)
      maybeMarketplaceResult <- liftEither eitherMaybeMaketplaceResult
      activePlan <-
        case maybeMarketplaceResult of
          Just marketplaceResult -> do
            let plan = marketplaceResult ^. #_plan
                maybePendingPlan = marketplaceResult ^. #_pendingPlan

            -- take the biggest limit plan so users can upgrade to imediately fix a limit problem
            pure $ maximum1 $ plan :| maybeToList maybePendingPlan
          Nothing -> do
            -- it seems for some reason certain installs are not on a plan
            -- are they really old and installed the app directly?
            -- or just some weird edge case I don't know about?
            -- ie. `jstark518` is one such example
            -- we default to the beta plan to avoid any disruption while this issue is investigated.
            emitAppEvent $ AppEvent AppEventLevelWarning (AppEventMessage "Plan Not Found") Nothing
            pure BetaPlan

      let result =
            DetermineAccountPlanResult
              { _plan = activePlan
              }
      emitAppEventInfoA (AppEventMessage "Finished: Determine Account Plan") (AppEventAdditional result)
      DetermineAccountPlanGitHubIOC $ pure (ctx $> result)
    (R other) -> DetermineAccountPlanGitHubIOC $ alg (runDetermineAccountPlanGitHubIOC . hdl) other ctx
