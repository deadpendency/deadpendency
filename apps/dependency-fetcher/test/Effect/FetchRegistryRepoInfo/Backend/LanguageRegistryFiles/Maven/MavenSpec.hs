{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenSpec (spec) where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Maven
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.CompareDependencyRegistryInfo
import Test.Hspec
import Text.Read (read)
import Text.URI qualified as URI

spec :: Spec
spec = parallel $
  context "when parsing real registry input" $ do
    context "java deps" $ do
      it "decodes the input correctly for springboot" $ do
        result <- fetchDependencyMaven Java (DependencyName "org.springframework.boot/spring-boot-starter-web")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "spring-projects") (RepoName "spring-boot"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "decodes the input correctly for javax.servlet-api" $ do
        result <- fetchDependencyMaven Java (DependencyName "javax.servlet/javax.servlet-api")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "javaee") (RepoName "servlet-spec"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "gracefully handles no repo" $ do
        result <- fetchDependencyMaven Java (DependencyName "com.google.inject/guice")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "some packages require the trailing slash" $ do
        result <- fetchDependencyMaven Java (DependencyName "com.fasterxml.jackson.core/jackson-databind")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "FasterXML") (RepoName "jackson-databind"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "detects relocation + subbing in artifact id" $ do
        result <- fetchDependencyMaven Java (DependencyName "dbunit/dbunit")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      (RASDeprecated RASTRelocated Nothing $ V.singleton $ DependencyName "org.dbunit/dbunit")
                      Nothing

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "ignores other similar #maven-metadata.xml.md5# versions" $ do
        result <- fetchDependencyMaven Java (DependencyName "org.apache.ivy/ivy")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoUnknown $ fromJust $ URI.mkURI "https://svn.apache.org/repos/asf/ant/ivy/core/trunk")
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "handles releases with only jar, no pom" $ do
        result <- fetchDependencyMaven Java (DependencyName "org.webjars.bower/datatables.net-responsive")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "handles weird dirs in the version list page" $ do
        result <- fetchDependencyMaven Java (DependencyName "info.cukes/cucumber-core")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      (RASDeprecated RASTRelocated Nothing (V.singleton $ DependencyName "io.cucumber/cucumber-core"))
                      Nothing

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "handles no jar link only pom" $ do
        result <- fetchDependencyMaven Java (DependencyName "org.springframework.roo/org.springframework.roo.annotations")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "handles blah" $ do
        result <- fetchDependencyMaven Java (DependencyName "net.ttddyy/datasource-assert")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "ttddyy") (RepoName "datasource-assert"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      -- This is actually broken now. Not planning on fixing..
      xit "handles mysql/mysql-connector-java which was renamed but they changed the old package too" $ do
        result <- fetchDependencyMaven Java (DependencyName "mysql/mysql-connector-java")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      (RASDeprecated RASTRelocated Nothing $ V.singleton $ DependencyName "com.mysql/mysql-connector-j")
                      Nothing

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "handles lastest release not having pom or jar" $ do
        result <- fetchDependencyMaven Java (DependencyName "org.cloudfoundry.identity/cloudfoundry-identity-login")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      Nothing
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "gracefully works with Kotlin" $ do
        result <- fetchDependencyMaven Kotlin (DependencyName "org.springframework.boot/spring-boot-starter-web")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "spring-projects") (RepoName "spring-boot"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "unexpected language throws an error" $ do
        result <- fetchDependencyMaven Python (DependencyName "org.springframework.boot/spring-boot-starter-web")

        let expected =
              Left (FDRRegistryFetchExceptional "Unexpected programming language to fetchLatestVersion: Python")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "gracefully handles 404" $ do
        result <- fetchDependencyMaven Java (DependencyName "no-way/not-exist-no-way-omg")

        let expected = Right Nothing

        result `shouldBe` expected

    context "scala deps" $ do
      it "happy day Scala dep" $ do
        result <- fetchDependencyMaven Scala (DependencyName "org.typelevel/cats-core")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "typelevel") (RepoName "cats"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "fetches old scala version deps" $ do
        result <- fetchDependencyMaven Scala (DependencyName "org.typelevel/frameless-refined")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "typelevel") (RepoName "frameless"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

      it "can fetch Java deps being loaded into a Scala project" $ do
        result <- fetchDependencyMaven Scala (DependencyName "org.springframework.boot/spring-boot-starter-web")

        let expected =
              Right $
                Just $
                  CompareDependencyRegistryInfo $
                    DependencyRegistryInfo
                      Maven
                      (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "spring-projects") (RepoName "spring-boot"))
                      RASAlive
                      (Just $ read "2020-06-01 00:00:00 UTC")

            compareResult = result <<&>> CompareDependencyRegistryInfo

        compareResult `shouldBe` expected

-- cannot find any example of this.. not sure what to do
-- it "handles relocation" $ do
--   result <- fetchDependencyMaven Scala (DependencyName "org.typelevel/cats")

--   let expected =
--         Right $
--           Just $
--             CompareDependencyRegistryInfo $
--               DependencyRegistryInfo
--                 Maven
--                 (Just $ RepoQR $ QualifiedRepo GitHub (RepoOwner "typelevel") (RepoName "cats"))
--                 (RASDeprecated RASTRelocated Nothing $ V.singleton $ DependencyName "org.typelevel/cats-core")
--                 (Just $ read "2020-06-01 00:00:00 UTC")

--       compareResult = result <<&>> CompareDependencyRegistryInfo

--   compareResult `shouldBe` expected
