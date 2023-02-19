module RF.Effect.FinishRun.Model.FinishRunRequest
  ( FinishRunRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.InterchangeEvent.RunResult
import Data.Aeson

newtype FinishRunRequest = FinishRunRequest
  { _runResult :: RunResult
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FinishRunRequest where
  toJSON = genericToJSON cleanJSONOptions
