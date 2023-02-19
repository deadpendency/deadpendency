module RP.Effect.ReadConfig.Model.ReadConfigError
  ( ReadConfigError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.ProcessingError
import Common.Model.Error.ToProcessingError
import Common.Model.Error.UserError
import Data.Aeson

newtype ReadConfigError
  = ParseConfigFailed Text
  deriving stock (Eq, Show, Generic)

instance ToProcessingError ReadConfigError where
  toProcessingError =
    \case
      ParseConfigFailed error' -> Just $ ProcessingErrorUser $ UserErrorInvalidConfig error'

instance ConsideredAppFailure ReadConfigError where
  consideredAppFailure =
    \case
      ParseConfigFailed _ -> True

instance ToJSON ReadConfigError where
  toJSON = genericToJSON cleanJSONOptions
