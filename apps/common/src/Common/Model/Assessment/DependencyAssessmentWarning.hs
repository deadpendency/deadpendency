module Common.Model.Assessment.DependencyAssessmentWarning
  ( DependencyAssessmentWarning (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessmentViolation
import Data.Aeson

newtype DependencyAssessmentWarning = DependencyAssessmentWarning
  { _violation :: DependencyAssessmentViolation
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyAssessmentWarning where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyAssessmentWarning where
  parseJSON = genericParseJSON cleanJSONOptions
