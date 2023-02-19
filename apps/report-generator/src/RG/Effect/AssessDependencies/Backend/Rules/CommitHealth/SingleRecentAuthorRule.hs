module RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.SingleRecentAuthorRule
  ( singleRecentAuthorRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

singleRecentAuthorRule :: Rule
singleRecentAuthorRule currentTime RulesConfig {_singleRecentAuthorRuleStatus = ruleStatus} _ theseRegistryRepo =
  case (ruleStatus, theseRegistryRepo ^? there) of
    (RSDisabled, _) -> InternalDependencyAssessment V.empty V.empty
    (_, Nothing) -> InternalDependencyAssessment V.empty V.empty
    (RSProduceWarning, Just repoStats) ->
      InternalDependencyAssessment (DependencyAssessmentWarning <$> produceViolation currentTime repoStats) V.empty
    (RSProduceFailure, Just repoStats) ->
      InternalDependencyAssessment V.empty (DependencyAssessmentFailure <$> produceViolation currentTime repoStats)

produceViolation :: UTCTime -> DependencyRepoStats -> V.Vector DependencyAssessmentViolation
produceViolation currentTime repoStats =
  let lastTwoYearsCommits = repoStats ^. #_twoYearlyCommitHistory
      oneYearAgoUTCTime = addUTCTime (negate $ nominalDay * 365) currentTime
      lastYearCommits = V.filter (\c -> c ^. #_commitDate > oneYearAgoUTCTime) lastTwoYearsCommits
      commitsAuthors = V.mapMaybe _commitAuthorEmail lastYearCommits
      maybeFirstCommitAuthor = safeHeadV commitsAuthors
   in -- no point alerting on this when the few yearly commits rule will alert
      if V.length lastYearCommits > 3
        then case maybeFirstCommitAuthor of
          Just commitAuthor ->
            if V.all (== commitAuthor) commitsAuthors
              then V.singleton DAVSingleRecentAuthor
              else V.empty
          Nothing -> V.empty
        else V.empty
