module DF.Effect.FetchDependencies.Model.FetchDependenciesRequest
  ( FetchDependenciesRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicRepoDependencies
import Data.Aeson

newtype FetchDependenciesRequest = FetchDependenciesRequest
  { _repositoryDependencies :: BasicRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FetchDependenciesRequest where
  toJSON = genericToJSON cleanJSONOptions
