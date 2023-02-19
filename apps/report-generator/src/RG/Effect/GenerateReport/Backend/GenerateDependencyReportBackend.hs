module RG.Effect.GenerateReport.Backend.GenerateDependencyReportBackend
  ( toMaybePass,
    toMaybeWarning,
    toMaybeFailure,
    toIgnoreReport,
  )
where

import Common.Model.Assessment.DependencyAssessment
import Common.Model.Assessment.DependencyAssessmentResult
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Dependency.Ignored.IgnoredDependency
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Report.DependencyFailureReport
import Common.Model.Report.DependencyIgnoreReport
import Common.Model.Report.DependencyPassReport
import Common.Model.Report.DependencyWarningReport
import Common.Model.Report.PackageLink

toMaybePass :: DependencyAssessment -> Maybe (ProgrammingLanguage, DependencyPassReport)
toMaybePass (DependencyAssessment e@(EnrichedDependency pl di _ _) DARPass) =
  Just
    ( pl,
      DependencyPassReport
        { _dependencyIdentifier = di,
          _dependencyRepo = getRepoFromEnriched e,
          _dependencyPackageLink = getPackageLinkFromEnriched e
        }
    )
toMaybePass _ = Nothing

toMaybeWarning :: DependencyAssessment -> Maybe (ProgrammingLanguage, DependencyWarningReport)
toMaybeWarning (DependencyAssessment e@(EnrichedDependency pl di _ _) (DARWarning depWarnings)) =
  Just
    ( pl,
      DependencyWarningReport
        { _dependencyIdentifier = di,
          _dependencyRepo = getRepoFromEnriched e,
          _dependencyPackageLink = getPackageLinkFromEnriched e,
          _dependencyAssessmentWarnings = depWarnings
        }
    )
toMaybeWarning _ = Nothing

toMaybeFailure :: DependencyAssessment -> Maybe (ProgrammingLanguage, DependencyFailureReport)
toMaybeFailure (DependencyAssessment e@(EnrichedDependency pl di _ _) (DARFailure depFailures depWarnings)) =
  Just
    ( pl,
      DependencyFailureReport
        { _dependencyIdentifier = di,
          _dependencyRepo = getRepoFromEnriched e,
          _dependencyPackageLink = getPackageLinkFromEnriched e,
          _dependencyAssessmentWarnings = depWarnings,
          _dependencyAssessmentFailures = depFailures
        }
    )
toMaybeFailure _ = Nothing

toIgnoreReport :: IgnoredDependency -> (ProgrammingLanguage, DependencyIgnoreReport)
toIgnoreReport (IgnoredDependency pl depIdentifier _) =
  ( pl,
    DependencyIgnoreReport depIdentifier
  )

getPackageLinkFromEnriched :: EnrichedDependency -> Maybe PackageLink
getPackageLinkFromEnriched ed =
  let dependencyIdentifier = ed ^. #_dependencyIdentifier
      maybeDependencyName = getDIName dependencyIdentifier
      maybeRegistry = ed ^? (#_data . here . #_registry)
   in case (maybeDependencyName, maybeRegistry) of
        (Just name, Just registry) ->
          Just $
            PackageLink
              registry
              name
        _ -> Nothing
