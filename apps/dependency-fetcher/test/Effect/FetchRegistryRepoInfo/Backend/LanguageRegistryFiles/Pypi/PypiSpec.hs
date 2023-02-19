{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.PypiSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.Pypi
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    it "decodes the input correctly for django" $ do
      result <- fetchDependencyPypi (DependencyName "django")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "django") (RepoName "django"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "decodes the input when no source url, but github homepage is present" $ do
      result <- fetchDependencyPypi (DependencyName "confuse")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "beetbox") (RepoName "confuse"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "gracefully handles 404" $ do
      result <- fetchDependencyPypi (DependencyName "not-exist-no-way-omg")

      let expected = Right Nothing

      result `shouldBe` expected

    it "handles 'Source Repository'" $ do
      result <- fetchDependencyPypi (DependencyName "numpy")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "numpy") (RepoName "numpy"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles 'Code'" $ do
      result <- fetchDependencyPypi (DependencyName "WTForms")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "wtforms") (RepoName "wtforms"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles 'Sources'" $ do
      result <- fetchDependencyPypi (DependencyName "Zope")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "zopefoundation") (RepoName "Zope"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles 'Repository'" $ do
      result <- fetchDependencyPypi (DependencyName "arrow")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "arrow-py") (RepoName "arrow"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles just 'Issue Tracker'" $ do
      result <- fetchDependencyPypi (DependencyName "SQLAlchemy")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "sqlalchemy") (RepoName "sqlalchemy"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles 'Source code'" $ do
      result <- fetchDependencyPypi (DependencyName "gunicorn")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "benoitc") (RepoName "gunicorn"))
                    RASAlive
                    (Just $ read "2020-06-01 00:00:00 UTC")

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected

    it "handles no content releases" $ do
      result <- fetchDependencyPypi (DependencyName "time")

      let expected =
            Right $
              Just $
                CompareDependencyRegistryInfo $
                  DependencyRegistryInfo
                    Pypi
                    Nothing
                    RASAlive
                    Nothing

          compareResult = result <<&>> CompareDependencyRegistryInfo

      compareResult `shouldBe` expected
