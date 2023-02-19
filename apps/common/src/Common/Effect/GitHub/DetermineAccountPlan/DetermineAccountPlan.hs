{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.DetermineAccountPlan.DetermineAccountPlan
  ( DetermineAccountPlan (..),
    accountPlanDetermine,
  )
where

import Common.Effect.GitHub.DetermineAccountPlan.Model.DetermineAccountPlanResult
import Control.Effect.TH

data DetermineAccountPlan (m :: Type -> Type) k where
  AccountPlanDetermine :: DetermineAccountPlan m DetermineAccountPlanResult

makeSmartConstructors ''DetermineAccountPlan
