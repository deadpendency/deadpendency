module Effect.AssessDependencies.Backend.Rules.CommitHealth.SingleRecentAuthorRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Common.Model.RepoConfig.Rules.RuleStatus
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.RepoConfig
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.SingleRecentAuthorRule
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "single recent author rule" $ do
    it "single recent author" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com")
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning DAVSingleRecentAuthor,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "single recent author, but configured for failure" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com")
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceFailure

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure DAVSingleRecentAuthor
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "3 or less commits do not trigger the rule" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com")
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "only commits in the last year trigger the rule" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2019-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2019-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2019-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2019-05-01 00:00:00 UTC") (Just "author@email.com")
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "multiple recent author" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "other-author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com")
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "no recent commits should pass" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory .~ V.empty
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "gracefully ignores nothings" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") Nothing,
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") Nothing,
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") Nothing
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSProduceWarning

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected

    it "false rules config results in pass" $
      hedgehog $ do
        dependencyRepoStats <- forAll genDependencyRepoStats
        rulesConfig <- Gen.sample genRulesConfig

        let currentTime = read "2020-06-01 00:00:00 UTC"
            massagedRepoStats =
              dependencyRepoStats
                & #_twoYearlyCommitHistory
                  .~ V.fromList
                    [ DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com"),
                      DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "author@email.com")
                    ]
            massagedRulesConfig =
              rulesConfig
                & #_singleRecentAuthorRuleStatus .~ RSDisabled

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = singleRecentAuthorRule currentTime massagedRulesConfig Nothing (That massagedRepoStats)

        result === expected
