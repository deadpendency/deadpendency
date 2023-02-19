module CRC.Model.AppError
  ( AppError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.CommonError (CommonError)
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.ToAppError
import Common.Model.Error.WriteChecksError
import Data.Aeson

data AppError
  = AppConfigLoadError
  | AppWriteChecksError WriteChecksError
  | AppCommonError CommonError
  deriving stock (Eq, Show, Generic)

instance ToAppError CommonError AppError where
  toAppError = AppCommonError

instance ToAppError WriteChecksError AppError where
  toAppError = AppWriteChecksError

instance ConsideredAppFailure AppError where
  consideredAppFailure =
    \case
      AppConfigLoadError -> True
      AppWriteChecksError e -> consideredAppFailure e
      AppCommonError e -> consideredAppFailure e

instance ToJSON AppError where
  toJSON = genericToJSON cleanJSONOptions
