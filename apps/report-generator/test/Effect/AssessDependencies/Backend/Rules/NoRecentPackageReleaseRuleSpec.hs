module Effect.AssessDependencies.Backend.Rules.NoRecentPackageReleaseRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.NoRecentPackageConfig
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.NoRecentPackageReleaseRule
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "recent commits rule" $ do
    it "with a recent release" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ read "2020-05-01 00:00:00 UTC"
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig ?~ NoRecentPackageConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "release within the warn range" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastReleaseDateTime = read "2020-02-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ lastReleaseDateTime
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig ?~ NoRecentPackageConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVNoRecentPackageRelease 3 lastReleaseDateTime,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "release in the error range" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastReleaseDateTime = read "2019-11-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ lastReleaseDateTime
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig ?~ NoRecentPackageConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVNoRecentPackageRelease 6 lastReleaseDateTime
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "with only warn enabled" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastReleaseDateTime = read "2019-02-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ lastReleaseDateTime
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig ?~ NoRecentPackageConfig (Just 3) Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVNoRecentPackageRelease 3 lastReleaseDateTime,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "with only error enabled" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastReleaseDateTime = read "2019-11-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ lastReleaseDateTime
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig ?~ NoRecentPackageConfig Nothing (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVNoRecentPackageRelease 6 lastReleaseDateTime
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "with rule fully disabled" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastReleaseDateTime = read "2019-11-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ lastReleaseDateTime
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig .~ Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected

    it "with both set to Nothing" $
      hedgehog $ do
        registryDetails <- forAll genDependencyRegistryInfo
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastReleaseDateTime = read "2019-11-01 00:00:00 UTC"
            massagedRegistryDetails =
              registryDetails
                & #_lastReleaseDateTime ?~ lastReleaseDateTime
            massagedRulesConfig =
              rulesConfig
                & #_noRecentPackageConfig ?~ NoRecentPackageConfig Nothing Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentPackageReleaseRule currentTime massagedRulesConfig Nothing (This massagedRegistryDetails)

        result === expected
