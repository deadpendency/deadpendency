{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.InternalSpec (spec) where

import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Git
import DF.Effect.FetchRegistryRepoInfo.Backend.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import DF.Effect.Model.FetchRegistryWithRepo
import Hedgehog.Gen qualified as Gen
import Test.Hspec

spec :: Spec
spec = parallel $
  context "produceResult" $ do
    it "error text results in errored dep" $ do
      basicDep <- Gen.sample genBasicDependency

      let eitherRegistryInfo =
            Left (FDRFailureToParseResult "error text")

          result = produceResult basicDep eitherRegistryInfo

          expected =
            Right $
              Left $
                ErroredDependency
                  (basicDep ^. #_dependencyIdentifier)
                  (basicDep ^. #_dependencyType)
                  (basicDep ^. #_programmingLanguage)
                  Nothing
                  (UnexpectedFailureToParseRegistryEntry "error text")

      result `shouldBe` expected

    it "data inconsistent results in errored dep" $ do
      basicDep <- Gen.sample genBasicDependency

      let eitherRegistryInfo =
            Left (FDRRegistryDataInconsistent "error text")

          result = produceResult basicDep eitherRegistryInfo

          expected =
            Right $
              Left $
                ErroredDependency
                  (basicDep ^. #_dependencyIdentifier)
                  (basicDep ^. #_dependencyType)
                  (basicDep ^. #_programmingLanguage)
                  Nothing
                  (UnexpectedFailureRegistryDataInconsistent "error text")

      result `shouldBe` expected

    it "no registry data (with named dep) results in errored dep" $ do
      basicDep <- Gen.sample genBasicDependency
      dependencyIdentifier <- Gen.sample genDependencyIdentifierNamed

      let massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ dependencyIdentifier

          eitherRegistryInfo =
            Right Nothing

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Left $
                ErroredDependency
                  (massagedBasicDep ^. #_dependencyIdentifier)
                  (massagedBasicDep ^. #_dependencyType)
                  (massagedBasicDep ^. #_programmingLanguage)
                  Nothing
                  NoRegistryOrRepoData

      result `shouldBe` expected

    it "no registry source repo results successful just registry info result" $ do
      basicDep <- Gen.sample genBasicDependency
      dependencyIdentifier <- Gen.sample genDependencyIdentifierNamed
      registryInfo <- Gen.sample genDependencyRegistryInfo

      let massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ dependencyIdentifier

          massagedRegistryInfo =
            registryInfo
              & #_sourceRepo .~ Nothing

      let eitherRegistryInfo =
            Right $ Just massagedRegistryInfo

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Right $
                FetchRegistryWithRepo
                  massagedBasicDep
                  (This massagedRegistryInfo)

      result `shouldBe` expected

    it "a github repo from registry results in successful dep" $ do
      basicDep <- Gen.sample genBasicDependency
      registryInfo <- Gen.sample genDependencyRegistryInfo
      dependencyIdentifier <- Gen.sample genDependencyIdentifierNamed
      qualifiedRepo <- Gen.sample genQualifiedRepo

      let massagedRepo =
            RepoQR $
              qualifiedRepo
                & #_repoHost .~ GitHub

          massagedRegistryInfo =
            registryInfo
              & #_sourceRepo ?~ massagedRepo

          massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ dependencyIdentifier

      let eitherRegistryInfo =
            Right $ Just massagedRegistryInfo

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Right $
                FetchRegistryWithRepo
                  massagedBasicDep
                  (These massagedRegistryInfo massagedRepo)

      result `shouldBe` expected

    it "a github repo from dep id (with registry info) results in successful dep" $ do
      basicDep <- Gen.sample genBasicDependency
      registryInfo <- Gen.sample genDependencyRegistryInfo
      dependencyIdentifier <- Gen.sample genDependencyIdentifierRepo
      qualifiedRepo <- Gen.sample genQualifiedRepo

      let massagedQualifiedRepo =
            qualifiedRepo
              & #_repoHost .~ GitHub

          massagedDependencyIdentifier =
            dependencyIdentifier
              & (_Ctor @"DependencyIdentifierRepo" . position @1) .~ massagedQualifiedRepo

          massagedRegistryInfo =
            registryInfo
              & #_sourceRepo .~ Nothing

          massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ massagedDependencyIdentifier

      let eitherRegistryInfo =
            Right $ Just massagedRegistryInfo

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Right $
                FetchRegistryWithRepo
                  massagedBasicDep
                  (These massagedRegistryInfo (RepoQR massagedQualifiedRepo))

      result `shouldBe` expected

    it "a github repo from dep id (with no registry info) results in successful dep" $ do
      basicDep <- Gen.sample genBasicDependency
      dependencyIdentifier <- Gen.sample genDependencyIdentifierRepo
      qualifiedRepo <- Gen.sample genQualifiedRepo

      let massagedQualifiedRepo =
            qualifiedRepo
              & #_repoHost .~ GitHub

          massagedDependencyIdentifier =
            dependencyIdentifier
              & (_Ctor @"DependencyIdentifierRepo" . position @1) .~ massagedQualifiedRepo

          massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ massagedDependencyIdentifier

      let eitherRegistryInfo =
            Right Nothing

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Right $
                FetchRegistryWithRepo
                  massagedBasicDep
                  (That $ RepoQR massagedQualifiedRepo)

      result `shouldBe` expected

    it "DI repo trumps registry repo" $ do
      basicDep <- Gen.sample genBasicDependency
      registryInfo <- Gen.sample genDependencyRegistryInfo
      dependencyIdentifier <- Gen.sample genDependencyIdentifierRepo
      diRepo <- Gen.sample genQualifiedRepo
      registryRepo <- Gen.sample genQualifiedRepo

      let massagedQualifiedDIRepo =
            diRepo
              & #_repoHost .~ GitHub

          massagedDependencyIdentifier =
            dependencyIdentifier
              & (_Ctor @"DependencyIdentifierRepo" . position @1) .~ massagedQualifiedDIRepo

          massagedRegistryInfo =
            registryInfo
              & #_sourceRepo ?~ RepoQR registryRepo

          massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ massagedDependencyIdentifier

      let eitherRegistryInfo =
            Right $ Just massagedRegistryInfo

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Right $
                FetchRegistryWithRepo
                  massagedBasicDep
                  (These massagedRegistryInfo (RepoQR massagedQualifiedDIRepo))

      result `shouldBe` expected

    it "a non GitHub repo from registry results in registry result" $ do
      basicDep <- Gen.sample genBasicDependency
      registryInfo <- Gen.sample genDependencyRegistryInfo
      dependencyIdentifier <- Gen.sample genDependencyIdentifierNamed
      qualifiedRepo <- Gen.sample genQualifiedRepo

      let massagedQualifiedRepo =
            qualifiedRepo
              & #_repoHost .~ Bitbucket

          massagedRegistryInfo =
            registryInfo
              & #_sourceRepo ?~ RepoQR massagedQualifiedRepo

          massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ dependencyIdentifier

      let eitherRegistryInfo =
            Right $ Just massagedRegistryInfo

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Right $
                FetchRegistryWithRepo
                  massagedBasicDep
                  (This massagedRegistryInfo)

      result `shouldBe` expected

    it "a non GitHub repo from DI (with no registry info) results in failed dep" $ do
      basicDep <- Gen.sample genBasicDependency
      dependencyIdentifier <- Gen.sample genDependencyIdentifierRepo
      qualifiedRepo <- Gen.sample genQualifiedRepo

      let massagedQualifiedRepo =
            qualifiedRepo
              & #_repoHost .~ Bitbucket

          massagedDependencyIdentifier =
            dependencyIdentifier
              & (_Ctor @"DependencyIdentifierRepo" . position @1) .~ massagedQualifiedRepo

          massagedBasicDep =
            basicDep
              & #_dependencyIdentifier .~ massagedDependencyIdentifier

      let eitherRegistryInfo =
            Right Nothing

          result = produceResult massagedBasicDep eitherRegistryInfo
          expected =
            Right $
              Left $
                ErroredDependency
                  (massagedBasicDep ^. #_dependencyIdentifier)
                  (massagedBasicDep ^. #_dependencyType)
                  (massagedBasicDep ^. #_programmingLanguage)
                  Nothing
                  NoRegistryOrRepoData

      result `shouldBe` expected
