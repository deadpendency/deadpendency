module Common.Effect.GitHub.DetermineAccountPlan.Carrier.DetermineAccountPlanRetC
  ( runDetermineAccountPlanRet,
  )
where

import Common.Effect.GitHub.DetermineAccountPlan.DetermineAccountPlan (DetermineAccountPlan (..))
import Common.Effect.GitHub.DetermineAccountPlan.Model.DetermineAccountPlanResult
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.Reader (ask)

newtype DetermineAccountPlanRetC m a = DetermineAccountPlanRetC {runDetermineAccountPlanRetC :: ReaderC DetermineAccountPlanResult m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (DetermineAccountPlan :+: sig) (DetermineAccountPlanRetC m) where
  alg hdl sig ctx = case sig of
    (L AccountPlanDetermine) -> DetermineAccountPlanRetC $ ask @DetermineAccountPlanResult <&> (<$ ctx)
    (R other) -> DetermineAccountPlanRetC $ alg (runDetermineAccountPlanRetC . hdl) (R other) ctx

runDetermineAccountPlanRet :: DetermineAccountPlanResult -> DetermineAccountPlanRetC m a -> m a
runDetermineAccountPlanRet t = runReader t . runDetermineAccountPlanRetC
