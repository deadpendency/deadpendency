module DF.Effect.FetchDependencies.Model.FetchDependenciesResult
  ( FetchDependenciesResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Enriched.EnrichedRepoDependencies
import Common.Model.Dependency.Errored.ErroredRepoDependencies
import Data.Aeson

data FetchDependenciesResult = FetchDependenciesResult
  { _enrichedRepoDependencies :: EnrichedRepoDependencies,
    _erroredRepoDependencies :: ErroredRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FetchDependenciesResult where
  toJSON = genericToJSON cleanJSONOptions
