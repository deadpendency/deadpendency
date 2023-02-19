module Effect.AssessDependencies.Backend.Rules.IsRepoNotFoundRuleSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.RepoConfig.Rules.RuleStatus
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Git
import CommonTest.Gen.Model.RepoConfig
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Hedgehog.Gen qualified as Gen
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.IsRepoNotFoundRule
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.URI qualified as URI

spec :: Spec
spec = parallel $
  context "is missing repo checking" $ do
    it "is missing repo, produce warning" $
      hedgehog $ do
        registryDetails <- Gen.sample genDependencyRegistryInfo
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig
        qualifiedRepo <- Gen.sample genQualifiedRepo

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotFound .~ RSProduceWarning

            massagedQualifiedRepo =
              qualifiedRepo
                & #_repoHost .~ GitHub

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.singleton $ DependencyAssessmentWarning DAVRepoNotFound,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotFoundRule currentTime massagedRulesConfig (Just $ RepoQR massagedQualifiedRepo) (This registryDetails)

        result === expected

    it "with repo will pass" $
      hedgehog $ do
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig
        qualifiedRepo <- Gen.sample genQualifiedRepo
        repoStats <- Gen.sample genDependencyRepoStats

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotFound .~ RSProduceWarning

            massagedQualifiedRepo =
              qualifiedRepo
                & #_repoHost .~ GitHub

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotFoundRule currentTime massagedRulesConfig (Just $ RepoQR massagedQualifiedRepo) (That repoStats)

        result === expected

    it "is missing repo, produce failure" $
      hedgehog $ do
        registryDetails <- Gen.sample genDependencyRegistryInfo
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig
        qualifiedRepo <- Gen.sample genQualifiedRepo

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotFound .~ RSProduceFailure

            massagedQualifiedRepo =
              qualifiedRepo
                & #_repoHost .~ GitHub

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.singleton $ DependencyAssessmentFailure DAVRepoNotFound
                }

            result = isRepoNotFoundRule currentTime massagedRulesConfig (Just $ RepoQR massagedQualifiedRepo) (This registryDetails)

        result === expected

    it "is missing repo, disabled" $
      hedgehog $ do
        registryDetails <- Gen.sample genDependencyRegistryInfo
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig
        qualifiedRepo <- Gen.sample genQualifiedRepo

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotFound .~ RSDisabled

            massagedQualifiedRepo =
              qualifiedRepo
                & #_repoHost .~ GitHub

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotFoundRule currentTime massagedRulesConfig (Just $ RepoQR massagedQualifiedRepo) (This registryDetails)

        result === expected

    it "is missing non github repo" $
      hedgehog $ do
        registryDetails <- Gen.sample genDependencyRegistryInfo
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig
        qualifiedRepo <- Gen.sample genQualifiedRepo

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotFound .~ RSProduceWarning

            massagedQualifiedRepo =
              qualifiedRepo
                & #_repoHost .~ Bitbucket

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotFoundRule currentTime massagedRulesConfig (Just $ RepoQR massagedQualifiedRepo) (This registryDetails)

        result === expected

    it "has an unknown repo" $
      hedgehog $ do
        registryDetails <- Gen.sample genDependencyRegistryInfo
        currentTime <- Gen.sample genUTCTime
        rulesConfig <- Gen.sample genRulesConfig

        let massagedRulesConfig =
              rulesConfig
                & #_repositoryNotFound .~ RSProduceWarning

            repo = RepoUnknown $ fromJust $ URI.mkURI "http://darcs.net/"

            expected =
              InternalDependencyAssessment
                { _idaDependencyAssessmentWarnings = V.empty,
                  _idaDependencyAssessmentFailures = V.empty
                }

            result = isRepoNotFoundRule currentTime massagedRulesConfig (Just repo) (This registryDetails)

        result === expected
