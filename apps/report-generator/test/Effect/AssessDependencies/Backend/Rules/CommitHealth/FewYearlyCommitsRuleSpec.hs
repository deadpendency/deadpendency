module Effect.AssessDependencies.Backend.Rules.CommitHealth.FewYearlyCommitsRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.RepoConfig.Rules.FewYearlyCommitsConfig
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.FewYearlyCommitsRule
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "few yearly commits rule" $ do
    it "has no yearly commits" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.empty
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig ?~ FewYearlyCommitsConfig (Just 7) (Just 3)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "more than warn at count commits" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        commits <- Gen.sample $ genVector (Range.constant 8 10) genDependencyRepoCommit
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            recentCommits = commits & (traversed . #_commitDate) .~ read "2019-07-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ recentCommits
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig ?~ FewYearlyCommitsConfig (Just 7) (Just 3)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "in the warn commits range" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        commits <- forAll $ genVector (Range.constant 4 7) genDependencyRepoCommit
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            recentCommits = commits & (traversed . #_commitDate) .~ read "2019-07-01 00:00:00 UTC"
            commitCount = V.length recentCommits
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ recentCommits
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig ?~ FewYearlyCommitsConfig (Just 7) (Just 3)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVFewYearlyCommits 7 commitCount,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "in the fail range" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        commits <- forAll $ genVector (Range.constant 1 3) genDependencyRepoCommit
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            recentCommits = commits & (traversed . #_commitDate) .~ read "2019-07-01 00:00:00 UTC"
            commitCount = V.length recentCommits
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ recentCommits
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig ?~ FewYearlyCommitsConfig (Just 7) (Just 3)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVFewYearlyCommits 3 commitCount
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "with no fail limit specified" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        commits <- forAll $ genVector (Range.constant 1 3) genDependencyRepoCommit
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            recentCommits = commits & (traversed . #_commitDate) .~ read "2019-07-01 00:00:00 UTC"
            commitCount = V.length recentCommits
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ recentCommits
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig ?~ FewYearlyCommitsConfig (Just 7) Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning $ DAVFewYearlyCommits 7 commitCount,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "config as nothing produces pass" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        commits <- forAll $ genVector (Range.constant 1 3) genDependencyRepoCommit
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            recentCommits = commits & (traversed . #_commitDate) .~ read "2019-07-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ recentCommits
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig .~ Nothing

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "only error produces error" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        commits <- forAll $ genVector (Range.constant 1 3) genDependencyRepoCommit
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            commitCount = V.length recentCommits
            recentCommits = commits & (traversed . #_commitDate) .~ read "2019-07-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ recentCommits
            massagedRulesConfig =
              rulesConfig
                & #_fewYearlyCommitsConfig ?~ FewYearlyCommitsConfig Nothing (Just 4)

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure $ DAVFewYearlyCommits 4 commitCount
                }

            result = fewYearlyCommitsRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected
