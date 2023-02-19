module Effect.AssessDependencies.Backend.Rules.CommitHealth.NoRecentCommitsRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Common.Model.RepoConfig.Rules.NoRecentCommitConfig
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.NoRecentCommitsRule
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "recent commits rule" $ do
    it "with recent commits" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.singleton (DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") Nothing)
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "with recent commits in the warn range" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastCommitTime = read "2020-02-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.singleton (DependencyRepoCommit lastCommitTime Nothing)
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVNoRecentCommits 3 (Just lastCommitTime),
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "with recent commits in the error range" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastCommitTime = read "2019-11-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.singleton (DependencyRepoCommit lastCommitTime Nothing)
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVNoRecentCommits 6 (Just lastCommitTime)
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "no commits in past 2 years" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.empty
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig (Just 3) (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVNoRecentCommits 6 Nothing
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "no config is a pass" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.singleton (DependencyRepoCommit (read "2020-02-01 00:00:00 UTC") Nothing)
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig .~ Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "just warn config produces a warn" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastCommitTime = read "2019-02-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.singleton (DependencyRepoCommit lastCommitTime Nothing)
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig (Just 3) Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVNoRecentCommits 3 (Just lastCommitTime),
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "just error config produces an error" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            lastCommitTime = read "2019-11-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.singleton (DependencyRepoCommit lastCommitTime Nothing)
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig Nothing (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVNoRecentCommits 6 (Just lastCommitTime)
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "just warn config and no commit is warn" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.empty
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig (Just 3) Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVNoRecentCommits 3 Nothing,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "just error config with no commits produces an error" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.empty
            massagedRulesConfig =
              rulesConfig
                & #_noRecentCommitConfig ?~ NoRecentCommitConfig Nothing (Just 6)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVNoRecentCommits 6 Nothing
                }

            result = noRecentCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected
