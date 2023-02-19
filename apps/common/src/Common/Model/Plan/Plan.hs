module Common.Model.Plan.Plan
  ( Plan (..),
    getPlanLimit,
    planIdToPlan,
    planToPlanId,
    planText,
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data Plan
  = BetaPlan
  | OpenSourceOrganizationPlan
  | IndividualDeveloperPlan
  | SmallPlan
  | MediumPlan
  | LargePlan
  deriving stock (Eq, Show, Generic, Ord)

instance ToJSON Plan where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON Plan where
  parseJSON = genericParseJSON cleanJSONOptions

planText :: Plan -> Text
planText =
  \case
    BetaPlan -> "beta"
    OpenSourceOrganizationPlan -> "opensource"
    IndividualDeveloperPlan -> "individualdeveloper"
    SmallPlan -> "small"
    MediumPlan -> "medium"
    LargePlan -> "large"

getPlanLimit :: Plan -> Maybe Int
getPlanLimit =
  \case
    BetaPlan -> Nothing
    OpenSourceOrganizationPlan -> Just 0
    IndividualDeveloperPlan -> Just 5
    SmallPlan -> Just 20
    MediumPlan -> Just 40
    LargePlan -> Just 100

planIdToPlan :: Int -> Maybe Plan
planIdToPlan =
  \case
    5541 -> Just BetaPlan
    6058 -> Just OpenSourceOrganizationPlan
    6777 -> Just IndividualDeveloperPlan
    7144 -> Just SmallPlan
    7145 -> Just MediumPlan
    7146 -> Just LargePlan
    _ -> Nothing

planToPlanId :: Plan -> Int
planToPlanId =
  \case
    BetaPlan -> 5541
    OpenSourceOrganizationPlan -> 6058
    IndividualDeveloperPlan -> 6777
    SmallPlan -> 7144
    MediumPlan -> 7145
    LargePlan -> 7146
