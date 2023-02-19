module Common.Model.GitHub.Checks.CheckRunStatus
  ( CheckRunStatus (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data CheckRunStatus
  = CheckRunStatusQueued
  | CheckRunStatusRequested
  | CheckRunStatusInProgress
  | CheckRunStatusCompleted
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON CheckRunStatus where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunStatus where
  parseJSON = genericParseJSON cleanJSONOptions
