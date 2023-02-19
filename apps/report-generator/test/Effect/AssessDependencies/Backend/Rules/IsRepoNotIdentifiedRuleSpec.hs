module Effect.AssessDependencies.Backend.Rules.IsRepoNotIdentifiedRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.RuleStatus
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Git
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.IsRepoNotIdentifiedRule
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "is missing repo checking" $ do
    it "is repo not identified, produce warning" $
      hedgehog $ do
        theseRegistryRepo <- Gen.sample (genThese genDependencyRegistryInfo genDependencyRepoStats)
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotIdentified .~ RSProduceWarning

        let expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning DAVRepoNotIdentified,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotIdentifiedRule currentTime massagedRulesConfig Nothing theseRegistryRepo

        result === expected

    it "repo is identified" $
      hedgehog $ do
        theseRegistryRepo <- Gen.sample (genThese genDependencyRegistryInfo genDependencyRepoStats)
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig
        repo <- Gen.sample genRepo

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotIdentified .~ RSProduceWarning

        let expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotIdentifiedRule currentTime massagedRulesConfig (Just repo) theseRegistryRepo

        result === expected

    it "is repo not identified, produce failure" $
      hedgehog $ do
        theseRegistryRepo <- Gen.sample (genThese genDependencyRegistryInfo genDependencyRepoStats)
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotIdentified .~ RSProduceFailure

        let expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure DAVRepoNotIdentified
                }

            result = isRepoNotIdentifiedRule currentTime massagedRulesConfig Nothing theseRegistryRepo

        result === expected

    it "is repo not identified, but disabled" $
      hedgehog $ do
        theseRegistryRepo <- Gen.sample (genThese genDependencyRegistryInfo genDependencyRepoStats)
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotIdentified .~ RSDisabled

        let expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotIdentifiedRule currentTime massagedRulesConfig Nothing theseRegistryRepo

        result === expected
