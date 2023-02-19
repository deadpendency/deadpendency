module Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateResult
  ( CheckRunUpdateResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.GitHub.Checks.CheckRun
import Data.Aeson

newtype CheckRunUpdateResult = CheckRunUpdateResult
  { _checkRun :: CheckRun
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunUpdateResult where
  toJSON = genericToJSON cleanJSONOptions
