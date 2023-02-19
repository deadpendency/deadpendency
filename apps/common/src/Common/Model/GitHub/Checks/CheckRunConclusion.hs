module Common.Model.GitHub.Checks.CheckRunConclusion
  ( CheckRunConclusion (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data CheckRunConclusion
  = CheckRunConclusionFailure
  | CheckRunConclusionSuccess
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON CheckRunConclusion where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunConclusion where
  parseJSON = genericParseJSON cleanJSONOptions
