module DF.Model.AppError
  ( AppError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.CommonError (CommonError)
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.FromAppError
import Common.Model.Error.ToAppError
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import Data.Aeson

data AppError
  = AppConfigLoadError
  | AppCommonError CommonError
  | AppFetchDependenciesError FetchDependenciesError
  deriving stock (Eq, Show, Generic)

instance ToAppError CommonError AppError where
  toAppError = AppCommonError

instance ToAppError FetchDependenciesError AppError where
  toAppError = AppFetchDependenciesError

instance FromAppError FetchDependenciesError AppError where
  fromAppError (AppFetchDependenciesError fetchDependenciesError) = Just fetchDependenciesError
  fromAppError _ = Nothing

instance ConsideredAppFailure AppError where
  consideredAppFailure =
    \case
      AppConfigLoadError -> True
      AppCommonError e -> consideredAppFailure e
      AppFetchDependenciesError e -> consideredAppFailure e

instance ToJSON AppError where
  toJSON = genericToJSON cleanJSONOptions
