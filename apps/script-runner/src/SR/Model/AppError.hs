module SR.Model.AppError
  ( AppError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.CommonError (CommonError)
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.ToAppError
import Data.Aeson

data AppError
  = AppConfigLoadError
  | AppCommonError CommonError
  | MissingRepoFileInput
  deriving stock (Eq, Show, Generic)

instance ToAppError CommonError AppError where
  toAppError = AppCommonError

instance ConsideredAppFailure AppError where
  consideredAppFailure =
    \case
      AppConfigLoadError -> True
      AppCommonError e -> consideredAppFailure e
      MissingRepoFileInput -> True

instance ToJSON AppError where
  toJSON = genericToJSON cleanJSONOptions
