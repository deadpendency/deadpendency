module Common.Model.Assessment.DependencyAssessment
  ( DependencyAssessment (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessmentResult
import Common.Model.Dependency.Enriched.EnrichedDependency
import Data.Aeson

data DependencyAssessment = DependencyAssessment
  { _enrichedDependency :: EnrichedDependency,
    _dependencyAssessmentResult :: DependencyAssessmentResult
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyAssessment where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyAssessment where
  parseJSON = genericParseJSON cleanJSONOptions
