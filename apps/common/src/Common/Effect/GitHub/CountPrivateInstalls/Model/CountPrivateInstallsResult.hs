module Common.Effect.GitHub.CountPrivateInstalls.Model.CountPrivateInstallsResult
  ( CountPrivateInstallsResult (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype CountPrivateInstallsResult = CountPrivateInstallsResult
  { _count :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CountPrivateInstallsResult where
  toJSON = genericToJSON cleanJSONOptions
