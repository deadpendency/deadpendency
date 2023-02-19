{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.PackagistSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.Packagist
import Data.Vector qualified as V
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for monolog" $ do
      result <- fetchDependencyPackagist (DependencyName "monolog/monolog")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Packagist
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "Seldaek") (RepoName "monolog"))
                    RASAlive
                    (Just $ read "2020-05-22 08:12:19 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "detects bool abandonment" $ do
      result <- fetchDependencyPackagist (DependencyName "frbit/message-signer")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Packagist
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "fortrabbit") (RepoName "message-signer"))
                    (RASDeprecated RASTAbandoned Nothing V.empty)
                    (Just $ read "2020-05-22 08:12:19 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "detects string message abandonment" $ do
      result <- fetchDependencyPackagist (DependencyName "sprov03/laravel-code-generator")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Packagist
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "sprov03") (RepoName "code-generator"))
                    (RASDeprecated RASTAbandoned (Just "No longer maintained") V.empty)
                    (Just $ read "2020-05-22 08:12:19 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "detects deprecated for abandonment" $ do
      result <- fetchDependencyPackagist (DependencyName "sllh/iso-codes-validator-bundle")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Packagist
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "Soullivaneuh") (RepoName "SLLHIsoCodesValidatorBundle"))
                    (RASDeprecated RASTAbandoned Nothing $ V.singleton $ DependencyName "sllh/iso-codes-validator")
                    (Just $ read "2020-05-22 08:12:19 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyPackagist (DependencyName "no-way/this-exists-omg")

      let expected = Right Nothing

      result `shouldBe` expected

    xit "release time play" $ do
      result <- fetchDependencyPackagist (DependencyName "laravel/framework")

      let expected =
            Right $
              Just $
                DependencyRegistryInfo
                  Packagist
                  (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "fortrabbit") (RepoName "message-signer"))
                  (RASDeprecated RASTAbandoned Nothing V.empty)
                  (Just $ read "2020-05-22 08:12:19 UTC")

      result `shouldBe` expected
