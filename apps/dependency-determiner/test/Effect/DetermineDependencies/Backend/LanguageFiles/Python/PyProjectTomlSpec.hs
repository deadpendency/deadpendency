module Effect.DetermineDependencies.Backend.LanguageFiles.Python.PyProjectTomlSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.PyProjectToml
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps PyProjectToml" $ do
    describe "base pyproject style deps" $ do
      it "determines simple named dependencies" $ do
        let input =
              [r|
[dependencies]
Click = "^7.0"

[dev-dependencies]
black = { version = "^18.3-alpha.0", python = "^3.6" }
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "Click") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "black") (Just DevDependency)
                    ]

        result `shouldBe` expected

      it "determines project dependencies" $ do
        let input =
              [r|
[project]
dependencies = [
    "requests",
    "requests_download",
]
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "requests") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "requests_download") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "handles build system deps" $ do
        let input =
              [r|
[build-system]
requires = [
    "setuptools >= 35.0.2",
    "setuptools_scm >= 2.0.0, <3",
    "just-name"
]
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "setuptools") (Just DevDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "setuptools_scm") (Just DevDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "just-name") (Just DevDependency)
                    ]

        result `shouldBe` expected

    it "handles optional deps" $ do
      let input =
            [r|
[project.optional-dependencies]
socks = [ 'PySocks >= 1.5.6, != 1.5.7, < 2' ]
tests = [
  'ddt >= 1.2.2, < 2',
  'pytest < 6',
  'mock >= 1.0.1, < 4; python_version < "3.4"',
]
|]
          result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
          expected =
            Right $
              V.fromList $
                sort
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "PySocks") (Just DevDependency),
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "ddt") (Just DevDependency),
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "pytest") (Just DevDependency),
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "mock") (Just DevDependency)
                  ]

      result `shouldBe` expected

    describe "poetry deps" $ do
      it "determines simple named dependencies" $ do
        let input =
              [r|
[tool.poetry.dependencies]
omg = "*"

[tool.poetry.dev-dependencies]
yeah = "^3.4"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "omg") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "yeah") (Just DevDependency)
                    ]

        result `shouldBe` expected

      it "determines git dependencies" $ do
        let input =
              [r|
[tool.poetry.dependencies]
omg = { git = "https://github.com/omg/omg.git" }

[tool.poetry.dev-dependencies]
yeah = { git = "https://github.com/yeah/yeah.git" }
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "omg") (RepoName "omg")) (Just $ DependencyName "omg")) (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "yeah") (RepoName "yeah")) (Just $ DependencyName "yeah")) (Just DevDependency)
                    ]

        result `shouldBe` expected

      it "handles no deps but has the keys" $ do
        let input =
              [r|
[tool.poetry.dependencies]

[tool.poetry.dev-dependencies]
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right V.empty

        result `shouldBe` expected

    describe "flit deps" $ do
      it "determines simple named dependencies" $ do
        let input =
              [r|
[tool.flit.metadata]
requires = ["tinydb==3.15.1", "configparser; python_version == '2.7'", "tabulate==0.8.7"]
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "tinydb") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "configparser") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "tabulate") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "works with required-extra deps" $ do
        let input =
              [r|
[tool.flit.metadata.requires-extra]
test = [ "pytest", "pytest-cov", ]
doc = ["sphinx"]
dev = ["omg"]
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PyProjectTomlInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "pytest") (Just DevDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "pytest-cov") (Just DevDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "sphinx") (Just DevDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "omg") (Just DevDependency)
                    ]

        result `shouldBe` expected
