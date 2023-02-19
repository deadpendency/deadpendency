module RF.Effect.FinishRun.Backend.Model.FinishCheckRun
  ( FinishCheckRun (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.Output.CheckRunOutputBody
import Common.Model.GitHub.Checks.Output.CheckRunOutputSummary
import Common.Model.GitHub.Checks.Output.CheckRunOutputTitle
import Data.Aeson

data FinishCheckRun = FinishCheckRun
  { _fcrSummary :: CheckRunOutputSummary,
    _fcrBody :: CheckRunOutputBody,
    _fcrTitle :: CheckRunOutputTitle,
    _fcrCheckRunConclusion :: CheckRunConclusion
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FinishCheckRun where
  toJSON = genericToJSON cleanJSONOptions
