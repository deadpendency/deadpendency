module Common.Model.Error.WriteChecksError (WriteChecksError (..)) where

import Common.Aeson.Aeson
import Common.Model.Error.ConsideredAppFailure
import Data.Aeson

data WriteChecksError
  = WCENetworkFailure Text
  | WCEBadRequest Text
  | WCEResponseDecodeFailure Text
  deriving stock (Eq, Show, Generic)

instance ConsideredAppFailure WriteChecksError where
  consideredAppFailure =
    \case
      _ -> True

instance ToJSON WriteChecksError where
  toJSON = genericToJSON cleanJSONOptions
