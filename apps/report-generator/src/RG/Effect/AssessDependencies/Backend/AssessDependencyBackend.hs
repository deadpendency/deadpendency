module RG.Effect.AssessDependencies.Backend.AssessDependencyBackend
  ( assessDependency,
  )
where

import Common.Model.Assessment.DependencyAssessment
import Common.Model.Assessment.DependencyAssessmentResult
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector.NonEmpty qualified as NV
import RG.Effect.AssessDependencies.Backend.Rules.Rules (rules)

assessDependency :: UTCTime -> RulesConfig -> EnrichedDependency -> DependencyAssessment
assessDependency currentTime rulesConfig enrichedDependency =
  let depData = enrichedDependency ^. #_data
      maybeRepo = getRepoFromEnriched enrichedDependency
      internalReport = foldMap' (\f -> f currentTime rulesConfig maybeRepo depData) rules
      warnings = internalReport ^. #_idaDependencyAssessmentWarnings
      failures = internalReport ^. #_idaDependencyAssessmentFailures
      assessmentResult =
        case NV.fromVector failures of
          Just nvFailures -> DARFailure nvFailures warnings
          Nothing -> maybe DARPass DARWarning (NV.fromVector warnings)
   in DependencyAssessment enrichedDependency assessmentResult
