module RP.Model.AppError
  ( AppError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.CommonError (CommonError)
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.FromAppError
import Common.Model.Error.ToAppError
import Common.Model.Error.WriteChecksError
import Data.Aeson
import RP.Effect.ReadConfig.Model.ReadConfigError

data AppError
  = AppConfigLoadError
  | AppWriteChecksError WriteChecksError
  | AppReadConfigError ReadConfigError
  | AppCommonError CommonError
  deriving stock (Eq, Show, Generic)

instance ToAppError CommonError AppError where
  toAppError = AppCommonError

instance ToAppError WriteChecksError AppError where
  toAppError = AppWriteChecksError

instance ToAppError ReadConfigError AppError where
  toAppError = AppReadConfigError

instance FromAppError ReadConfigError AppError where
  fromAppError (AppReadConfigError readConfigError) = Just readConfigError
  fromAppError _ = Nothing

instance ConsideredAppFailure AppError where
  consideredAppFailure =
    \case
      AppConfigLoadError -> True
      AppWriteChecksError e -> consideredAppFailure e
      AppReadConfigError e -> consideredAppFailure e
      AppCommonError e -> consideredAppFailure e

instance ToJSON AppError where
  toJSON = genericToJSON cleanJSONOptions
