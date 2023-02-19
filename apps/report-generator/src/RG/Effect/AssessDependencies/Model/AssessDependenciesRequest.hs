module RG.Effect.AssessDependencies.Model.AssessDependenciesRequest
  ( AssessDependenciesRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Enriched.EnrichedRepoDependencies
import Data.Aeson

newtype AssessDependenciesRequest = AssessDependenciesRequest
  { _enrichedRepoDependencies :: EnrichedRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AssessDependenciesRequest where
  toJSON = genericToJSON cleanJSONOptions
