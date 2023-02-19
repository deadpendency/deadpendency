module Common.Effect.GitHub.DetermineAccountPlan.Model.DetermineAccountPlanResult
  ( DetermineAccountPlanResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Plan.Plan
import Data.Aeson

newtype DetermineAccountPlanResult = DetermineAccountPlanResult
  { _plan :: Plan
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DetermineAccountPlanResult where
  toJSON = genericToJSON cleanJSONOptions
