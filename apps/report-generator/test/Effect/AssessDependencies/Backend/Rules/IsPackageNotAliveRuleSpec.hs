module Effect.AssessDependencies.Backend.Rules.IsPackageNotAliveRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.RepoConfig.Rules.RuleStatus
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.IsPackageNotAliveRule
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "is package deprecated rule checking" $ do
    it "is package deprecated" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        maybeDeprecationReason <- forAll $ Gen.maybe genAlphaText
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRegistryDetails =
              registryDetails
                & #_alivenessStatus .~ RASDeprecated RASTDeprecated maybeDeprecationReason V.empty
            massagedRulesConfig =
              rulesConfig
                & #_packageDeprecatedRuleStatus .~ RSProduceFailure
            registry =
              registryDetails ^. #_registry

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVPackageDeprecated registry DAVDTDeprecated maybeDeprecationReason V.empty
                }

            result = isPackageNotAliveRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "is package deprecated, but produce warning" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        maybeDeprecationReason <- forAll $ Gen.maybe genAlphaText
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRegistryDetails =
              registryDetails
                & #_alivenessStatus .~ RASDeprecated RASTDeprecated maybeDeprecationReason V.empty
            massagedRulesConfig =
              rulesConfig
                & #_packageDeprecatedRuleStatus .~ RSProduceWarning
            registry =
              registryDetails ^. #_registry

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVPackageDeprecated registry DAVDTDeprecated maybeDeprecationReason V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isPackageNotAliveRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "is package abandoned" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        maybeDeprecationReason <- forAll $ Gen.maybe genAlphaText
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRegistryDetails =
              registryDetails
                & #_alivenessStatus .~ RASDeprecated RASTAbandoned maybeDeprecationReason V.empty
            massagedRulesConfig =
              rulesConfig
                & #_packageDeprecatedRuleStatus .~ RSProduceFailure
            registry =
              registryDetails ^. #_registry

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVPackageDeprecated registry DAVDTAbandoned maybeDeprecationReason V.empty
                }

            result = isPackageNotAliveRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "is package relocated" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        newDependencyName <- forAll genDependencyName
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRegistryDetails =
              registryDetails
                & #_registry .~ Maven
                & #_alivenessStatus .~ RASDeprecated RASTRelocated Nothing (V.singleton newDependencyName)
            massagedRulesConfig =
              rulesConfig
                & #_packageDeprecatedRuleStatus .~ RSProduceFailure

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVPackageDeprecated Maven DAVDTRelocated Nothing (V.singleton newDependencyName)
                }

            result = isPackageNotAliveRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "is package alive" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRegistryDetails =
              registryDetails
                & #_alivenessStatus .~ RASAlive
            massagedRulesConfig =
              rulesConfig
                & #_packageDeprecatedRuleStatus .~ RSProduceFailure
            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isPackageNotAliveRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "is package deprecated but rules is false" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        maybeDeprecationReason <- forAll $ Gen.maybe genAlphaText
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRegistryDetails =
              registryDetails
                & #_alivenessStatus .~ RASDeprecated RASTDeprecated maybeDeprecationReason V.empty
            massagedRulesConfig =
              rulesConfig
                & #_packageDeprecatedRuleStatus .~ RSDisabled

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isPackageNotAliveRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected
