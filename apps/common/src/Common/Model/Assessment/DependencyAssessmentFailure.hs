module Common.Model.Assessment.DependencyAssessmentFailure
  ( DependencyAssessmentFailure (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessmentViolation
import Data.Aeson

newtype DependencyAssessmentFailure = DependencyAssessmentFailure
  { _violation :: DependencyAssessmentViolation
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyAssessmentFailure where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyAssessmentFailure where
  parseJSON = genericParseJSON cleanJSONOptions
