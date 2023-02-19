module RG.Effect.AssessDependencies.Backend.Rules.IsArchivedRule
  ( isArchivedRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

isArchivedRule :: Rule
isArchivedRule _ RulesConfig {_repositoryArchivedRuleStatus = ruleStatus} _ theseRegistryRepo =
  case (ruleStatus, theseRegistryRepo ^? there) of
    (RSDisabled, _) -> InternalDependencyAssessment V.empty V.empty
    (_, Nothing) -> InternalDependencyAssessment V.empty V.empty
    (RSProduceWarning, Just repoStats) ->
      InternalDependencyAssessment (DependencyAssessmentWarning <$> produceViolation repoStats) V.empty
    (RSProduceFailure, Just repoStats) ->
      InternalDependencyAssessment V.empty (DependencyAssessmentFailure <$> produceViolation repoStats)

produceViolation :: DependencyRepoStats -> V.Vector DependencyAssessmentViolation
produceViolation repoStats =
  if repoStats ^. #_isArchived
    then V.singleton DAVRepoArchived
    else V.empty
