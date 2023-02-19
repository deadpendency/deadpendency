{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevRepositorySpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevRepository
import Test.Hspec

spec :: Spec
spec = parallel $
  context "when inferring golang source repository" $ do
    describe "loading a github dep" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "github.com/Azure/go-autorest")

            expected =
              QualifiedRepo GitHub (RepoOwner "Azure") (RepoName "go-autorest")

        result `shouldBe` Just expected

      it "with trailing bits" $ do
        let result = determineSourceRepository (DependencyName "github.com/Azure/go-autorest/autorest")

            expected =
              QualifiedRepo GitHub (RepoOwner "Azure") (RepoName "go-autorest")

        result `shouldBe` Just expected

    describe "loading gopkg.in dep" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "gopkg.in/go-playground/validator.v9")

            expected =
              QualifiedRepo GitHub (RepoOwner "go-playground") (RepoName "validator")

        result `shouldBe` Just expected

      it "single name case" $ do
        let result = determineSourceRepository (DependencyName "gopkg.in/sourcemap")

            expected =
              QualifiedRepo GitHub (RepoOwner "go-sourcemap") (RepoName "sourcemap")

        result `shouldBe` Just expected

      it "single name case with version" $ do
        let result = determineSourceRepository (DependencyName "gopkg.in/sourcemap.v1")

            expected =
              QualifiedRepo GitHub (RepoOwner "go-sourcemap") (RepoName "sourcemap")

        result `shouldBe` Just expected

    describe "loading k8s.io" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "k8s.io/client-go")

            expected =
              QualifiedRepo GitHub (RepoOwner "kubernetes") (RepoName "client-go")

        result `shouldBe` Just expected

      it "sigs case " $ do
        let result = determineSourceRepository (DependencyName "sigs.k8s.io/controller-runtime")

            expected =
              QualifiedRepo GitHub (RepoOwner "kubernetes-sigs") (RepoName "controller-runtime")

        result `shouldBe` Just expected

    describe "loading rsc.io" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "rsc.io/quote")

            expected =
              QualifiedRepo GitHub (RepoOwner "rsc") (RepoName "quote")

        result `shouldBe` Just expected

      it "with version as name" $ do
        let result = determineSourceRepository (DependencyName "rsc.io/quote/v3")

            expected =
              QualifiedRepo GitHub (RepoOwner "rsc") (RepoName "quote")

        result `shouldBe` Just expected

    describe "loading go.uber.org" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "go.uber.org/zap")

            expected =
              QualifiedRepo GitHub (RepoOwner "go-uber") (RepoName "zap")

        result `shouldBe` Just expected

    describe "loading golang.org" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "golang.org/x/net/context")

            expected =
              QualifiedRepo GitHub (RepoOwner "golang") (RepoName "net")

        result `shouldBe` Just expected

    describe "loading go.etcd.io" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "go.etcd.io/bbolt")

            expected =
              QualifiedRepo GitHub (RepoOwner "etcd-io") (RepoName "bbolt")

        result `shouldBe` Just expected

    describe "loading gorm.io" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "gorm.io/gorm/logger")

            expected =
              QualifiedRepo GitHub (RepoOwner "go-gorm") (RepoName "gorm")

        result `shouldBe` Just expected

      it "driver case" $ do
        let result = determineSourceRepository (DependencyName "gorm.io/driver/mysql")

            expected =
              QualifiedRepo GitHub (RepoOwner "go-gorm") (RepoName "mysql")

        result `shouldBe` Just expected

    describe "unknown go dep" $ do
      it "happy day" $ do
        let result = determineSourceRepository (DependencyName "awesomego.io/cool/yeah")

        result `shouldBe` Nothing
