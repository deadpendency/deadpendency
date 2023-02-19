module Common.Model.InterchangeEvent.DependenciesFetched
  ( DependenciesFetched (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Enriched.EnrichedRepoDependencies
import Common.Model.Dependency.Errored.ErroredRepoDependencies
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Data.Aeson

data DependenciesFetched = DependenciesFetched
  { _enrichedRepoDependencies :: EnrichedRepoDependencies,
    _erroredRepoDependencies :: ErroredRepoDependencies,
    _ignoredRepoDependencies :: IgnoredRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependenciesFetched where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependenciesFetched where
  parseJSON = genericParseJSON cleanJSONOptions
