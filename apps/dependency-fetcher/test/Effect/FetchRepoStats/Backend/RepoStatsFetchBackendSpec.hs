{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRepoStats.Backend.RepoStatsFetchBackendSpec (spec) where

import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import CommonTest.Gen.Model.Dependency
import DF.Effect.FetchRepoStats.Backend.RepoStatsFetchBackend
import DF.Effect.FetchRepoStats.Model.DependencyFetchResult
import DF.Effect.Model.FetchRegistryWithRepo
import Data.HashMap.Strict qualified as HM
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "toDependencyFetchResult" $ do
    it "with regsitry + repo match finds all data in ED" $
      hedgehog $ do
        dependencyIdentifier <- forAll genDependencyIdentifier
        basicDep <- Gen.sample genBasicDependency
        registryInfo <- Gen.sample genDependencyRegistryInfo
        repoStats <- Gen.sample genDependencyRepoStats

        let massagedBasicDep =
              basicDep
                & #_dependencyIdentifier .~ dependencyIdentifier

            repo = RepoQR $ QualifiedRepo GitHub (RepoOwner "owner") (RepoName "name")

            hmRepoStats = HM.fromList [("repo-github-owner-name", repoStats)]
            theseData = These registryInfo repo
            registryWithRepo = FetchRegistryWithRepo massagedBasicDep theseData

            result = toDependencyFetchResult hmRepoStats registryWithRepo

            expected =
              DFRSuccess $
                EnrichedDependency
                  (massagedBasicDep ^. #_programmingLanguage)
                  dependencyIdentifier
                  (massagedBasicDep ^. #_dependencyType)
                  (These registryInfo repoStats)

        result === expected

    it "missing repo but has registry succeeds as ED with this" $
      hedgehog $ do
        dependencyIdentifier <- forAll genDependencyIdentifier
        basicDep <- Gen.sample genBasicDependency
        registryInfo <- Gen.sample genDependencyRegistryInfo

        let massagedBasicDep =
              basicDep
                & #_dependencyIdentifier .~ dependencyIdentifier

            hmRepoStats = HM.fromList []
            theseData = This registryInfo
            registryWithRepo = FetchRegistryWithRepo massagedBasicDep theseData

            result = toDependencyFetchResult hmRepoStats registryWithRepo

            expected =
              DFRSuccess $
                EnrichedDependency
                  (massagedBasicDep ^. #_programmingLanguage)
                  dependencyIdentifier
                  (massagedBasicDep ^. #_dependencyType)
                  (This registryInfo)

        result === expected

    it "missing repo + registry results in errored" $
      hedgehog $ do
        dependencyIdentifier <- forAll genDependencyIdentifier
        basicDep <- Gen.sample genBasicDependency

        let massagedBasicDep =
              basicDep
                & #_dependencyIdentifier .~ dependencyIdentifier

            repo = RepoQR $ QualifiedRepo GitHub (RepoOwner "owner") (RepoName "name")

            hmRepoStats = HM.fromList []
            theseData = That repo
            registryWithRepo = FetchRegistryWithRepo massagedBasicDep theseData

            result = toDependencyFetchResult hmRepoStats registryWithRepo

            expected =
              DFRErrored $
                ErroredDependency
                  dependencyIdentifier
                  (massagedBasicDep ^. #_dependencyType)
                  (massagedBasicDep ^. #_programmingLanguage)
                  Nothing
                  NoRegistryOrRepoData

        result === expected
