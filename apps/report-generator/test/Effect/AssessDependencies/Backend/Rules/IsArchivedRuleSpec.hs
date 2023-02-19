module Effect.AssessDependencies.Backend.Rules.IsArchivedRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.RuleStatus
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.IsArchivedRule
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "is archived rule checking" $ do
    it "is archived" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isArchived .~ True
            massagedRulesConfig =
              rulesConfig
                & #_repositoryArchivedRuleStatus .~ RSProduceFailure

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure DAVRepoArchived
                }

            result = isArchivedRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "is archived, but produce warning" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isArchived .~ True
            massagedRulesConfig =
              rulesConfig
                & #_repositoryArchivedRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning DAVRepoArchived,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isArchivedRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "is not archived" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isArchived .~ False
            massagedRulesConfig =
              rulesConfig
                & #_repositoryArchivedRuleStatus .~ RSProduceFailure

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isArchivedRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "is archived but no rules config is false" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isArchived .~ True
            massagedRulesConfig =
              rulesConfig
                & #_repositoryArchivedRuleStatus .~ RSDisabled

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isArchivedRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected
