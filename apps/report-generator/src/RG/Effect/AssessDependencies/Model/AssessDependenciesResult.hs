module RG.Effect.AssessDependencies.Model.AssessDependenciesResult
  ( AssessDependenciesResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Assessment.DependencyAssessment
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

newtype AssessDependenciesResult = AssessDependenciesResult
  { _dependencyAssessments :: NV.NonEmptyVector DependencyAssessment
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AssessDependenciesResult where
  toJSON = genericToJSON cleanJSONOptions
