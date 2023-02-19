module Effect.AssessDependencies.Backend.Rules.IsForkRuleSpec (spec) where

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
import RG.Effect.AssessDependencies.Backend.Rules.IsForkRule
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "is fork rule checking" $ do
    it "is fork" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isFork .~ True
            massagedRulesConfig =
              rulesConfig
                & #_repositoryIsForkRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning DAVIsFork,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isForkRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "is fork, but produce failure" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isFork .~ True
            massagedRulesConfig =
              rulesConfig
                & #_repositoryIsForkRuleStatus .~ RSProduceFailure

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure DAVIsFork
                }

            result = isForkRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "is not fork" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isFork .~ False
            massagedRulesConfig =
              rulesConfig
                & #_repositoryIsForkRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isForkRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "is fork but rules config is false" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRepoStats =
              dependencyRepoStats
                & #_isFork .~ True
            massagedRulesConfig =
              rulesConfig
                & #_repositoryIsForkRuleStatus .~ RSDisabled

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isForkRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected
