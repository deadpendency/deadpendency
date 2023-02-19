module RG.Effect.AssessDependencies.Backend.Rules.IsRepoNotFoundRule
  ( isRepoNotFoundRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

isRepoNotFoundRule :: Rule
isRepoNotFoundRule _ RulesConfig {_repositoryNotFound = ruleStatus} maybeRepo theseRegistryRepo =
  case ruleStatus of
    RSDisabled -> InternalDependencyAssessment V.empty V.empty
    RSProduceWarning ->
      InternalDependencyAssessment (DependencyAssessmentWarning <$> produceViolation maybeRepo theseRegistryRepo) V.empty
    RSProduceFailure ->
      InternalDependencyAssessment V.empty (DependencyAssessmentFailure <$> produceViolation maybeRepo theseRegistryRepo)

produceViolation :: Maybe Repo -> These DependencyRegistryInfo DependencyRepoStats -> V.Vector DependencyAssessmentViolation
produceViolation maybeRepo theseRegistryRepo =
  case (maybeRepo, theseRegistryRepo ^? there) of
    (Just (RepoQR QualifiedRepo {_repoHost = GitHub}), Nothing) -> V.singleton DAVRepoNotFound
    _ -> V.empty
