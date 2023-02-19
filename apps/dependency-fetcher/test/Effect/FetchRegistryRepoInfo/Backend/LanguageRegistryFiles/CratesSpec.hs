{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CratesSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Crates.Crates
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for rand" $ do
      result <- fetchDependencyCrates (DependencyName "rand")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Crates
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "rust-random") (RepoName "rand"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyCrates (DependencyName "not-exist-no-way-omg")

      let expected = Right Nothing

      result `shouldBe` expected
