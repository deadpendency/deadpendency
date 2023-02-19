{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDev
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for for github repo" $ do
      result <- fetchDependencyPkgGoDev (DependencyName "github.com/Azure/go-autorest/autorest")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    PkgGoDev
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "Azure") (RepoName "go-autorest"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles packages with no releases" $ do
      result <- fetchDependencyPkgGoDev (DependencyName "gopkg.in/yaml.v1")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    PkgGoDev
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "go-yaml") (RepoName "yaml"))
                    RASAlive
                    Nothing

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully ignores unknown package" $ do
      result <- fetchDependencyPkgGoDev (DependencyName "not-exists/no-way-no-how")

      let expected = Right Nothing

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected
