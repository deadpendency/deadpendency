{-# LANGUAGE TemplateHaskell #-}

module RP.Effect.VerifyPlan.VerifyPlan
  ( VerifyPlan (..),
    planVerify,
  )
where

import Control.Effect.TH

data VerifyPlan (m :: Type -> Type) k where
  PlanVerify :: VerifyPlan m ()

makeSmartConstructors ''VerifyPlan
