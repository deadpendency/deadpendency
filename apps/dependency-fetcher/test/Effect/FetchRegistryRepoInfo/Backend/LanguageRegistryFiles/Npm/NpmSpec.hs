{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.NpmSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.Npm
import Data.Vector qualified as V
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for react-dom" $ do
      result <- fetchDependencyNPM (DependencyName "react-dom")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "facebook") (RepoName "react"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles weird dep name chars" $ do
      result <- fetchDependencyNPM (DependencyName "@testing-library/jest-dom")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "testing-library") (RepoName "jest-dom"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "detects deprecation correctly" $ do
      result <- fetchDependencyNPM (DependencyName "nomnom")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "harthur") (RepoName "nomnom"))
                    (RASDeprecated RASTDeprecated (Just "Package no longer supported. Contact support@npmjs.com for more info.") V.empty)
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles deprecation as boolean" $ do
      result <- fetchDependencyNPM (DependencyName "react-scripts")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "facebook") (RepoName "create-react-app"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles deprecation as repository as string" $ do
      result <- fetchDependencyNPM (DependencyName "@types/react-joyride")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "gilbarbara") (RepoName "react-joyride"))
                    (RASDeprecated RASTDeprecated (Just "This is a stub types definition. react-joyride provides its own type definitions, so you do not need this installed.") V.empty)
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles no repository" $ do
      result <- fetchDependencyNPM (DependencyName "find-project-root")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    Nothing
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles www.github.com" $ do
      result <- fetchDependencyNPM (DependencyName "@types/express-ws")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "DefinitelyTyped") (RepoName "DefinitelyTyped"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles no latest" $ do
      result <- fetchDependencyNPM (DependencyName "@4geit/rct-login2-component")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    Nothing
                    RASAlive
                    Nothing

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles no dist-tags" $ do
      result <- fetchDependencyNPM (DependencyName "language-json")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Npm
                    Nothing
                    RASAlive
                    Nothing

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyNPM (DependencyName "not-exist-no-way-omg")

      let expected = Right Nothing

      result `shouldBe` expected
