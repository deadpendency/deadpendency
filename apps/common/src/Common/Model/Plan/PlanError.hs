module Common.Model.Plan.PlanError
  ( PlanError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Plan.Plan
import Data.Aeson

type PrivateRepoCount = Int

data PlanError
  = ExceededPlan Plan PrivateRepoCount
  deriving stock (Eq, Show, Generic)

instance ToJSON PlanError where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON PlanError where
  parseJSON = genericParseJSON cleanJSONOptions
