module RG.Effect.AssessDependencies.Backend.Rules.IsForkRule
  ( isForkRule,
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

isForkRule :: Rule
isForkRule _ RulesConfig {_repositoryIsForkRuleStatus = ruleStatus} _ theseRegistryRepo =
  case (ruleStatus, theseRegistryRepo ^? there) of
    (RSDisabled, _) -> InternalDependencyAssessment V.empty V.empty
    (_, Nothing) -> InternalDependencyAssessment V.empty V.empty
    (RSProduceWarning, Just dependencyRepoStats) ->
      InternalDependencyAssessment (DependencyAssessmentWarning <$> produceViolation dependencyRepoStats) V.empty
    (RSProduceFailure, Just dependencyRepoStats) ->
      InternalDependencyAssessment V.empty (DependencyAssessmentFailure <$> produceViolation dependencyRepoStats)

produceViolation :: DependencyRepoStats -> V.Vector DependencyAssessmentViolation
produceViolation repoStats =
  if repoStats ^. #_isFork
    then V.singleton DAVIsFork
    else V.empty
