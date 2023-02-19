{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGemsSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGems
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for rails" $ do
      result <- fetchDependencyRubyGems (DependencyName "rails")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    { _registry = RubyGems,
                      _sourceRepo = Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "rails") (RepoName "rails"),
                      _alivenessStatus = RASAlive,
                      _lastReleaseDateTime = Just (read "2020-05-22 08:12:19 UTC")
                    }

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles missing repo" $ do
      result <- fetchDependencyRubyGems (DependencyName "gitlab-qa")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    { _registry = RubyGems,
                      _sourceRepo = Nothing,
                      _alivenessStatus = RASAlive,
                      _lastReleaseDateTime = Just (read "2020-05-22 08:12:19 UTC")
                    }

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "loads source repo from the homepage_uri as well" $ do
      result <- fetchDependencyRubyGems (DependencyName "pg")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    { _registry = RubyGems,
                      _sourceRepo = Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "ged") (RepoName "ruby-pg"),
                      _alivenessStatus = RASAlive,
                      _lastReleaseDateTime = Just (read "2020-05-22 08:12:19 UTC")
                    }

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "loads source repo correctly when both are set" $ do
      result <- fetchDependencyRubyGems (DependencyName "bootsnap")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    { _registry = RubyGems,
                      _sourceRepo = Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "Shopify") (RepoName "bootsnap"),
                      _alivenessStatus = RASAlive,
                      _lastReleaseDateTime = Just (read "2020-05-22 08:12:19 UTC")
                    }

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyRubyGems (DependencyName "not-exist-no-way-omg")

      let expected = Right Nothing

      result `shouldBe` expected
