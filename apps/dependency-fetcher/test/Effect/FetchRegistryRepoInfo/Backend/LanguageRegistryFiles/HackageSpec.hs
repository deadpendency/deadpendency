{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.HackageSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.Hackage
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)
import Text.URI qualified as URI

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for real package" $ do
      result <- fetchDependencyHackage (DependencyName "req")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "mrkkrp") (RepoName "req"))
                    RASAlive
                    (Just $ read "2020-06-15 15:06:07 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "decodes the input correctly for a git repo reference" $ do
      result <- fetchDependencyHackage (DependencyName "servant")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "haskell-servant") (RepoName "servant"))
                    RASAlive
                    (Just $ read "2020-06-15 15:06:07 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handle all kinds of line endings" $ do
      result <- fetchDependencyHackage (DependencyName "random")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "haskell") (RepoName "random"))
                    RASAlive
                    (Just $ read "2020-06-15 15:06:07 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "works with text after the repo name" $ do
      result <- fetchDependencyHackage (DependencyName "weeder")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "ocharles") (RepoName "weeder"))
                    RASAlive
                    (Just $ read "2020-06-15 15:06:07 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "falls back to `homepage` when the source repo is not listed" $ do
      result <- fetchDependencyHackage (DependencyName "http-client")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "snoyberg") (RepoName "http-client"))
                    RASAlive
                    (Just $ read "2020-06-15 15:06:07 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully ignores non git repos" $ do
      result <- fetchDependencyHackage (DependencyName "darcs")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoUnknown $ fromJust $ URI.mkURI "http://darcs.net/")
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles unusual casing on cabal file" $ do
      result <- fetchDependencyHackage (DependencyName "warp")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "yesodweb") (RepoName "wai"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "detects deprecation" $ do
      result <- fetchDependencyHackage (DependencyName "cabal")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Hackage
                    Nothing
                    (RASDeprecated RASTDeprecated Nothing $ V.singleton $ DependencyName "Cabal")
                    (Just $ read "2020-06-15 15:06:07 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyHackage (DependencyName "not-exist-no-way-omg")

      let expected = Right Nothing

      result `shouldBe` expected
