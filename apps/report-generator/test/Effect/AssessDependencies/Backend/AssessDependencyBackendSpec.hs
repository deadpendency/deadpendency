module Effect.AssessDependencies.Backend.AssessDependencyBackendSpec (spec) where

import Common.Model.Assessment.DependencyAssessment
import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentResult
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Ecosystem.Registry
import Common.Model.RepoConfig.Rules.RulesConfig
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Git
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RG.Effect.AssessDependencies.Backend.AssessDependencyBackend
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "generating dependency report" $
    it "unhealthy dependencies produce a fail" $
      hedgehog $ do
        enrichedDependency <- forAll genEnrichedDependency
        registryInfo <- Gen.sample genDependencyRegistryInfo
        repo <- Gen.sample genRepo
        repoStats <- Gen.sample genDependencyRepoStats
        commits <- Gen.sample $ genVector (Range.constant 10 10) genDependencyRepoCommit

        let currentTime = read "2020-06-01 00:00:00 UTC"
            recentCommits = commits & (traversed . #_commitDate) .~ read "2020-04-01 00:00:00 UTC"
            withLatestCommits = DependencyRepoCommit (read "2020-05-01 00:00:00 UTC") (Just "blah@email.com") `V.cons` recentCommits
            massagedEnrichedDependency =
              enrichedDependency
                & #_programmingLanguage .~ Haskell
                & #_dependencyIdentifier .~ DependencyIdentifierNamed (DependencyName "dep-name")
                & #_data
                  .~ These
                    ( registryInfo
                        & #_registry .~ Hackage
                        & #_alivenessStatus .~ RASAlive
                        & #_lastReleaseDateTime ?~ currentTime
                        & #_sourceRepo ?~ repo
                    )
                    ( repoStats
                        & #_twoYearlyCommitHistory .~ withLatestCommits
                        & #_isArchived .~ True
                        & #_isFork .~ False
                    )

            expected =
              DependencyAssessment
                { _enrichedDependency = massagedEnrichedDependency,
                  _dependencyAssessmentResult = DARFailure (NV.singleton $ DependencyAssessmentFailure DAVRepoArchived) V.empty
                }

            result = assessDependency currentTime defaultRulesConfig massagedEnrichedDependency

        result === expected
