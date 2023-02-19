module RF.Effect.FinishRun.Model.FinishRunResult
  ( FinishRunResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.GitHub.Checks.CheckRun
import Data.Aeson

newtype FinishRunResult = FinishRunResult
  { _checkRun :: CheckRun
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FinishRunResult where
  toJSON = genericToJSON cleanJSONOptions
