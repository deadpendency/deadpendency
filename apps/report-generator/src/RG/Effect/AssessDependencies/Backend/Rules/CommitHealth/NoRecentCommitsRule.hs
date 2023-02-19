module RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.NoRecentCommitsRule
  ( noRecentCommitsRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

noRecentCommitsRule :: Rule
noRecentCommitsRule _ RulesConfig {_noRecentCommitConfig = Nothing} _ _ = InternalDependencyAssessment V.empty V.empty
noRecentCommitsRule currentTime RulesConfig {_noRecentCommitConfig = Just commitConfig} _ theseRegistryRepo =
  case theseRegistryRepo ^? there of
    Nothing -> InternalDependencyAssessment V.empty V.empty
    Just repoStats ->
      let maybeWarnAtMonths = commitConfig ^. #_warnAtMonths
          maybeFailAtMonths = commitConfig ^. #_failAtMonths
          maybeLastCommitDate = repoStats ^? (#_twoYearlyCommitHistory . _head . #_commitDate)
          toTime x = addUTCTime (negate $ nominalDay * 30 * realToFrac x) currentTime
       in case (maybeLastCommitDate, maybeWarnAtMonths, maybeFailAtMonths) of
            (_, Nothing, Nothing) -> InternalDependencyAssessment V.empty V.empty
            (Nothing, Just warnAtMonths, Nothing) -> InternalDependencyAssessment (V.singleton $ DependencyAssessmentWarning $ DAVNoRecentCommits warnAtMonths Nothing) V.empty
            (Nothing, _, Just failAtMonths) -> InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVNoRecentCommits failAtMonths Nothing))
            (Just lastCommitDate, Nothing, Just failAtMonths) ->
              if lastCommitDate < toTime failAtMonths
                then InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVNoRecentCommits failAtMonths (Just lastCommitDate)))
                else InternalDependencyAssessment V.empty V.empty
            (Just lastCommitDate, Just warnAtMonths, Nothing) ->
              if lastCommitDate < toTime warnAtMonths
                then InternalDependencyAssessment (V.singleton $ DependencyAssessmentWarning $ DAVNoRecentCommits warnAtMonths (Just lastCommitDate)) V.empty
                else InternalDependencyAssessment V.empty V.empty
            (Just lastCommitDate, Just warnAtMonths, Just failAtMonths) ->
              if
                  | lastCommitDate >= toTime warnAtMonths ->
                      InternalDependencyAssessment V.empty V.empty
                  | lastCommitDate >= toTime failAtMonths ->
                      InternalDependencyAssessment (V.singleton $ DependencyAssessmentWarning $ DAVNoRecentCommits warnAtMonths (Just lastCommitDate)) V.empty
                  | otherwise -> InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVNoRecentCommits failAtMonths (Just lastCommitDate)))
