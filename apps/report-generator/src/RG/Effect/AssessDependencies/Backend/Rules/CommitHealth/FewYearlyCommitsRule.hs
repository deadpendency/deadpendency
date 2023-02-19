module RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.FewYearlyCommitsRule
  ( fewYearlyCommitsRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

fewYearlyCommitsRule :: Rule
fewYearlyCommitsRule _ RulesConfig {_fewYearlyCommitsConfig = Nothing} _ _ = InternalDependencyAssessment V.empty V.empty
fewYearlyCommitsRule currentTime RulesConfig {_fewYearlyCommitsConfig = Just commitConfig} _ theseRegistryRepo =
  case theseRegistryRepo ^? (there . #_twoYearlyCommitHistory) of
    Nothing -> InternalDependencyAssessment V.empty V.empty
    Just lastTwoYearsCommits ->
      let oneYearAgoUTCTime = addUTCTime (negate $ nominalDay * 365) currentTime
          lastYearCommits = V.filter (\c -> c ^. #_commitDate > oneYearAgoUTCTime) lastTwoYearsCommits
          lastYearCommitCount = V.length lastYearCommits
          maybeWarnAtCount = commitConfig ^. #_warnAtCount
          maybeFailAtCount = commitConfig ^. #_failAtCount
       in -- this rule is not relevent with no commits. The recent commits rule covers that scenario
          if lastYearCommitCount == 0
            then InternalDependencyAssessment V.empty V.empty
            else case (maybeWarnAtCount, maybeFailAtCount) of
              (Nothing, Nothing) -> InternalDependencyAssessment V.empty V.empty
              (Just warnAtCount, Nothing) ->
                if lastYearCommitCount <= warnAtCount
                  then InternalDependencyAssessment (V.singleton (DependencyAssessmentWarning $ DAVFewYearlyCommits warnAtCount lastYearCommitCount)) V.empty
                  else InternalDependencyAssessment V.empty V.empty
              (Nothing, Just failAtCount) ->
                if lastYearCommitCount <= failAtCount
                  then InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVFewYearlyCommits failAtCount lastYearCommitCount))
                  else InternalDependencyAssessment V.empty V.empty
              (Just warnAtCount, Just failAtCount) ->
                if
                    | lastYearCommitCount > warnAtCount -> InternalDependencyAssessment V.empty V.empty
                    | lastYearCommitCount > failAtCount -> InternalDependencyAssessment (V.singleton (DependencyAssessmentWarning $ DAVFewYearlyCommits warnAtCount lastYearCommitCount)) V.empty
                    | otherwise -> InternalDependencyAssessment V.empty (V.singleton (DependencyAssessmentFailure $ DAVFewYearlyCommits failAtCount lastYearCommitCount))
