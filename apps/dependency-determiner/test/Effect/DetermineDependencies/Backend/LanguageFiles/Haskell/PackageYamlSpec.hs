module Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.PackageYamlSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.PackageYaml
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a PackageYaml" $ do
      it "determines happy day dependencies" $ do
        let inputRequirements =
              [r|
name: check-run-creator

dependencies:
  - lens
  - text

library:
  source-dirs: src
  dependencies:
    - servant
    - servant-server
    - either
    - lens

tests:
  check-run-creator-test:
    main: Spec.hs
    source-dirs: test
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - check-run-creator

      - servant-server
      - fused-effects
      - github
      - text
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "lens") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "text") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "servant") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "servant-server") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "either") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fused-effects") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "github") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles multiple test packages" $ do
        let inputRequirements =
              [r|
name: check-run-creator

library:
  source-dirs: src
  dependencies:
    - servant
    - servant-server
    - either

tests:
  check-run-creator-test:
    dependencies:
      - check-run-creator

      - servant-server
      - fused-effects
      - github

  zintegration-test:
    dependencies:
      - check-run-creator

      - validation
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "servant") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "servant-server") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "either") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fused-effects") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "github") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "validation") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles bench packages" $ do
        let inputRequirements =
              [r|
name: check-run-creator

benchmarks:
  package-load:
    source-dirs: bench
    main: Main.hs
    ghc-options:
      - -rtsopts
      - -with-rtsopts=-hm
    dependencies:
      - check-run-creator

      - nonempty-vector
      - vector
      - deepseq
      - weigh
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "nonempty-vector") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "vector") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "deepseq") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "weigh") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles exe packages" $ do
        let inputRequirements =
              [r|
name: check-run-creator

executables:
  dependency-determiner-exe:
    main: Main.hs
    source-dirs: app
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - check-run-creator

      - nonempty-vector
      - vector
      - deepseq
      - weigh
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "nonempty-vector") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "vector") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "deepseq") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "weigh") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles no tests" $ do
        let inputRequirements =
              [r|
name: check-run-creator

dependencies:
  - lens
  - text

library:
  source-dirs: src
  dependencies:
    - servant
    - servant-server
    - either
    - lens
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "lens") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "text") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "servant") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "servant-server") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "either") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles no library deps" $ do
        let inputRequirements =
              [r|
name: check-run-creator

dependencies:
  - base

  - hlint
  - weeder

library:
  source-dirs: src
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "hlint") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "weeder") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "versions are ignored" $ do
        let inputRequirements =
              [r|
name: check-run-creator

dependencies:
  - base >=4.7 && <5

  - mtl >=2.2
  - transformers >=0.5

library:
  source-dirs: src
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "mtl") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "transformers") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles only executables" $ do
        let inputRequirements =
              [r|
name: check-run-creator

dependencies:
  - base >=4.7 && <5

  - mtl >=2.2
  - transformers >=0.5

executables:
  hledger-ui:
    source-dirs: .
    main: hledger-ui.hs
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "mtl") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "transformers") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles no deps at all" $ do
        let inputRequirements =
              [r|
name: check-run-creator

library:
  source-dirs: src
|]
            input = PackageYamlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected
