module Effect.DetermineDependencies.Backend.LanguageFiles.Scala.BuildSbtSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Scala.BuildSbt
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a BuildSbt file" $ do
      it "determines happy day dependencies" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.14"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.apache.pdfbox/pdfbox") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "commons-io/commons-io") Nothing
                  ]

        result `shouldBe` expected

      it "handles test dependencies" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.14" % "test"
libraryDependencies += "commons-io" % "commons-io" % "2.6" % "test"
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.apache.pdfbox/pdfbox") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "commons-io/commons-io") Nothing
                  ]

        result `shouldBe` expected

      it "works with double percent dependencies" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies += "org.apache.pdfbox" %% "pdfbox" % "2.0.14"
libraryDependencies += "commons-io" %% "commons-io" % "2.6"
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.apache.pdfbox/pdfbox") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "commons-io/commons-io") Nothing
                  ]

        result `shouldBe` expected

      it "works with seq style dependencies" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies ++= Seq(
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.4",
  "org.scalatest" % "scalatest" % "1.9.1" % "test",

  "org.foobar" %% "foobar" % "1.8"
)
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "net.sourceforge.htmlcleaner/htmlcleaner") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalatest/scalatest") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.foobar/foobar") Nothing
                  ]

        result `shouldBe` expected

      it "gracefully ignores variable dependencies" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies ++= Seq(
  filters,
  evolutions,
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.4",
  "org.scalatest" % "scalatest" % "1.9.1" % "test",
  "org.foobar" %% "foobar" % "1.8",
  otherone
)

libraryDependencies += guice
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "net.sourceforge.htmlcleaner/htmlcleaner") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalatest/scalatest") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.foobar/foobar") Nothing
                  ]

        result `shouldBe` expected

      it "works with single line seq style dependencies" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies ++= Seq("net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.4", "org.scalatest" % "scalatest" % "1.9.1" % "test", "org.foobar" %% "foobar" % "1.8")

libraryDependencies ++= Seq("net.sourceforge.htmlcleaner" % "otherone" % "2.4")
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "net.sourceforge.htmlcleaner/htmlcleaner") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalatest/scalatest") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.foobar/foobar") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "net.sourceforge.htmlcleaner/otherone") Nothing
                  ]

        result `shouldBe` expected

      it "works with multiple seq style dependencies in a row" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies ++= Seq("net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.4", "org.scalatest" % "scalatest" % "1.9.1" % "test", "org.foobar" %% "foobar" % "1.8")
libraryDependencies ++= Seq("net.sourceforge.htmlcleaner" % "otherone" % "2.4")
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "net.sourceforge.htmlcleaner/htmlcleaner") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalatest/scalatest") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.foobar/foobar") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "net.sourceforge.htmlcleaner/otherone") Nothing
                  ]

        result `shouldBe` expected

      it "works with seq mapping version" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

circeVersion := "1.0.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic"
).map(_ % circeVersion)
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "io.circe/circe-core") Nothing,
                    BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "io.circe/circe-generic") Nothing
                  ]

        result `shouldBe` expected

      it "handles dep in settings style" $ do
        let inputRequirements =
              [r|
lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-example-project",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",
    scalaVersion := "3.1.3",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalameta/munit") Nothing
                  ]

        result `shouldBe` expected

      it "handles seq style in settings" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file("."))
  .settings(
    name := "scala-cats-examples",
    libraryDependencies ++= Seq(
      "org.scalaj" %% "scalaj-http" % "2.4.1"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.1"),
    scalacOptions += "-Ypartial-unification",
  )
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalaj/scalaj-http") Nothing
                  ]

        result `shouldBe` expected

      it "removes scala versions from dep names" $ do
        let inputRequirements =
              [r|
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Scala (DependencyIdentifierNamed $ DependencyName "org.scalatest/scalatest") Nothing
                  ]

        result `shouldBe` expected

      it "works with no dependencies" $ do
        let inputRequirements =
              [r|
no deps anywhere here
|]
            input = BuildSbtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected
