module RP.Effect.VerifyPlan.Carrier.VerifyPlanRetC
  ( runVerifyPlanRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import RP.Effect.VerifyPlan.VerifyPlan (VerifyPlan (..))

newtype VerifyPlanRetC m a = VerifyPlanRetC {runVerifyPlanRetC :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (VerifyPlan :+: sig) (VerifyPlanRetC m) where
  alg hdl sig ctx = case sig of
    (L PlanVerify) -> VerifyPlanRetC $ pure $ ctx $> ()
    (R other) -> VerifyPlanRetC $ alg (runVerifyPlanRetC . hdl) other ctx

runVerifyPlanRet :: VerifyPlanRetC m a -> m a
runVerifyPlanRet = runVerifyPlanRetC
