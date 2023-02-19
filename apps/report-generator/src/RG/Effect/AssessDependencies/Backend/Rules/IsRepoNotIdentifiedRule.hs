module RG.Effect.AssessDependencies.Backend.Rules.IsRepoNotIdentifiedRule
  ( isRepoNotIdentifiedRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Git.Repo
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

isRepoNotIdentifiedRule :: Rule
isRepoNotIdentifiedRule _ RulesConfig {_repositoryNotIdentified = ruleStatus} maybeRepo _ =
  case ruleStatus of
    RSDisabled -> InternalDependencyAssessment V.empty V.empty
    RSProduceWarning ->
      InternalDependencyAssessment (DependencyAssessmentWarning <$> produceViolation maybeRepo) V.empty
    RSProduceFailure ->
      InternalDependencyAssessment V.empty (DependencyAssessmentFailure <$> produceViolation maybeRepo)

produceViolation :: Maybe Repo -> V.Vector DependencyAssessmentViolation
produceViolation maybeRepo =
  case maybeRepo of
    Nothing -> V.singleton DAVRepoNotIdentified
    Just _ -> V.empty
