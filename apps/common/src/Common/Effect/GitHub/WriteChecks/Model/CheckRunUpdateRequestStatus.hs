module Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequestStatus
  ( CheckRunUpdateRequestStatus (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data CheckRunUpdateRequestStatus
  = CheckRunUpdateRequestStatusQueued
  | CheckRunUpdateRequestStatusInProgress
  | CheckRunUpdateRequestStatusCompleted
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunUpdateRequestStatus where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunUpdateRequestStatus where
  parseJSON = genericParseJSON cleanJSONOptions
