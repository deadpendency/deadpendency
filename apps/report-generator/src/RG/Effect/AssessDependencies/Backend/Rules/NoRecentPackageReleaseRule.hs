module RG.Effect.AssessDependencies.Backend.Rules.NoRecentPackageReleaseRule
  ( noRecentPackageReleaseRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

noRecentPackageReleaseRule :: Rule
noRecentPackageReleaseRule _ RulesConfig {_noRecentPackageConfig = Nothing} _ _ = InternalDependencyAssessment V.empty V.empty
noRecentPackageReleaseRule currentTime RulesConfig {_noRecentPackageConfig = Just recentPackageConfig} _ theseData =
  case theseData ^? (here . #_lastReleaseDateTime . _Just) of
    Nothing -> InternalDependencyAssessment V.empty V.empty
    Just latestReleaseDateTime ->
      let maybeWarnAtMonths = recentPackageConfig ^. #_warnAtMonths
          maybeFailAtMonths = recentPackageConfig ^. #_failAtMonths
          toTime x = addUTCTime (negate $ nominalDay * 30 * realToFrac x) currentTime
       in case (maybeWarnAtMonths, maybeFailAtMonths) of
            (Nothing, Nothing) -> InternalDependencyAssessment V.empty V.empty
            (Just warnAtMonths, Nothing) ->
              if latestReleaseDateTime < toTime warnAtMonths
                then InternalDependencyAssessment (V.singleton (DependencyAssessmentWarning $ DAVNoRecentPackageRelease warnAtMonths latestReleaseDateTime)) V.empty
                else InternalDependencyAssessment V.empty V.empty
            (Nothing, Just failAtMonths) ->
              if latestReleaseDateTime < toTime failAtMonths
                then InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVNoRecentPackageRelease failAtMonths latestReleaseDateTime))
                else InternalDependencyAssessment V.empty V.empty
            (Just warnAtMonths, Just failAtMonths) ->
              if
                  | latestReleaseDateTime >= toTime warnAtMonths ->
                      InternalDependencyAssessment V.empty V.empty
                  | latestReleaseDateTime >= toTime failAtMonths ->
                      InternalDependencyAssessment (V.singleton (DependencyAssessmentWarning $ DAVNoRecentPackageRelease warnAtMonths latestReleaseDateTime)) V.empty
                  | otherwise -> InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVNoRecentPackageRelease failAtMonths latestReleaseDateTime))
