module Common.Model.Assessment.DependencyAssessmentResult
  ( DependencyAssessmentResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentWarning
import Data.Aeson
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

data DependencyAssessmentResult
  = DARPass
  | DARWarning (NV.NonEmptyVector DependencyAssessmentWarning)
  | DARFailure (NV.NonEmptyVector DependencyAssessmentFailure) (V.Vector DependencyAssessmentWarning)
  deriving stock (Eq, Show, Generic)

instance ToJSON DependencyAssessmentResult where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyAssessmentResult where
  parseJSON = genericParseJSON cleanJSONOptions
