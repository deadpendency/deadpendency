{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGetSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGet
import Data.Vector qualified as V
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for Castle.Core" $ do
      result <- fetchDependencyNuGet (DependencyName "Castle.Core")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    NuGet
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "castleproject") (RepoName "Core"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "detects deprecation correctly" $ do
      result <- fetchDependencyNuGet (DependencyName "NuGet.Core")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    NuGet
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "NuGet") (RepoName "NuGet2"))
                    (RASDeprecated RASTDeprecated (Just "NuGet.Core is part of NuGet client v2 APIs. They have been replaced by NuGet client v3 and later APIs. https://docs.microsoft.com/en-us/nuget/reference/nuget-client-sdk") V.empty)
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles deprecation with no message + deprecated for" $ do
      result <- fetchDependencyNuGet (DependencyName "NuGet.Protocol.Core.Types")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    NuGet
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "NuGet") (RepoName "NuGet.Client"))
                    (RASDeprecated RASTDeprecated Nothing $ V.singleton $ DependencyName "NuGet.Protocol")
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles . in repo name" $ do
      result <- fetchDependencyNuGet (DependencyName "Ardalis.EFCore.Extensions")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    NuGet
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "ardalis") (RepoName "EFCore.Extensions"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyNuGet (DependencyName "not-exist-no-way-omg")

      let expected = Right Nothing

      result `shouldBe` expected
