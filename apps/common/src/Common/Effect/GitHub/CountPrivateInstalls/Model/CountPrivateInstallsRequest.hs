module Common.Effect.GitHub.CountPrivateInstalls.Model.CountPrivateInstallsRequest
  ( CountPrivateInstallsRequest (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype CountPrivateInstallsRequest = CountPrivateInstallsRequest
  { _planLimit :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CountPrivateInstallsRequest where
  toJSON = genericToJSON cleanJSONOptions
