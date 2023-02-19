module DD.Model.AppError
  ( AppError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.CommonError (CommonError)
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.FromAppError
import Common.Model.Error.ToAppError
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Aeson

data AppError
  = AppConfigLoadError
  | AppCommonError CommonError
  | AppDetermineDependenciesError DetermineDependenciesError
  | MissingRepoFileInput
  deriving stock (Eq, Show, Generic)

instance ToAppError DetermineDependenciesError AppError where
  toAppError = AppDetermineDependenciesError

instance FromAppError DetermineDependenciesError AppError where
  fromAppError (AppDetermineDependenciesError readConfigError) = Just readConfigError
  fromAppError _ = Nothing

instance ToAppError CommonError AppError where
  toAppError = AppCommonError

instance ConsideredAppFailure AppError where
  consideredAppFailure =
    \case
      AppConfigLoadError -> True
      AppCommonError e -> consideredAppFailure e
      AppDetermineDependenciesError e -> consideredAppFailure e
      MissingRepoFileInput -> True

instance ToJSON AppError where
  toJSON = genericToJSON cleanJSONOptions
