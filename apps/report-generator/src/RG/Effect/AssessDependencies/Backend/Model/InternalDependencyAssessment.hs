module RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
  ( InternalDependencyAssessment (..),
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentWarning
import Data.Vector qualified as V

data InternalDependencyAssessment = InternalDependencyAssessment
  { _idaDependencyAssessmentWarnings :: V.Vector DependencyAssessmentWarning,
    _idaDependencyAssessmentFailures :: V.Vector DependencyAssessmentFailure
  }
  deriving stock (Eq, Show, Generic)

instance Semigroup InternalDependencyAssessment where
  (InternalDependencyAssessment warnings1 errors1) <> (InternalDependencyAssessment warnings2 errors2) =
    InternalDependencyAssessment (warnings1 <> warnings2) (errors1 <> errors2)

instance Monoid InternalDependencyAssessment where
  mempty = InternalDependencyAssessment V.empty V.empty
