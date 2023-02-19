module Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.HaskellCabalSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.HaskellCabal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a HaskellCabal" $ do
      it "determines happy day dependencies" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
version: 0.0.1

name: semantic-core
library
  build-depends:
      base                      >= 4.13 && < 5
    , fused-effects               ^>= 1.1
    , fused-syntax
    , parsers                     ^>= 0.12.10
    , prettyprinter                >= 1.2.1 && < 2

test-suite test
  type: exitcode-stdio-1.0
  main-is:        Test.hs
  build-depends:
      base
    , semantic-core
    , fused-effects
    , fused-syntax
    , hedgehog ^>= 1
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fused-effects") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fused-syntax") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "parsers") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "prettyprinter") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "hedgehog") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles multiple test packages" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
library
  build-depends:
      base                         >= 4.13 && < 5
    , servant               ^>= 1.1
    , servant-server
    , either                     ^>= 0.12.10

test-suite test-a
  type: exitcode-stdio-1.0
  main-is:        Test.hs
  build-depends:
      base
    , check-run-creator
    , servant-server
    , fused-effects
    , github ^>= 1

test-suite test-b
  type: exitcode-stdio-1.0
  main-is:        Test.hs
  build-depends:
      base
    , check-run-creator
    , validation
|]
            input = HaskellCabalInput inputRequirements
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

      it "handles no tests" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
library
  build-depends:
      lens                         >= 4.13 && < 5
    , text
    , servant               ^>= 1.1
    , servant-server
    , either                     ^>= 0.12.10
|]
            input = HaskellCabalInput inputRequirements
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

      it "handles bench deps" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
benchmark bench-file-parsers-space

  default-language:
    Haskell2010

  main-is:
    Space.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -threaded

  hs-source-dirs:
    lib/file-parsers/bench

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "file-parsers") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "case-insensitive") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "criterion") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "deepseq") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "filepath") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles exe deps" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
executable pcg

  default-language:
    Haskell2010

  main-is:
    Main.hs

  -- Try to build a static binary, not sure if these flags work
  -- Can also try these flags: -optl-static -optl-pthread
  ghc-options:
    -threaded -rtsopts

  ld-options: -static

  hs-source-dirs:
    app/pcg

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "file-parsers") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "case-insensitive") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "criterion") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "deepseq") (Just DevDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "filepath") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles one line build depends" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
library
  build-depends: base, text, lens
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "text") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "lens") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles common stanza dependencies" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
common my-first-common-stanza
  build-depends:    foo ^>= 4.13
library
  import:        my-first-common-stanza
  build-depends: text ^>= 1.2.4.0
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "foo") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "text") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles sub libraries" $ do
        let inputRequirements =
              [r|cabal-version: 3.0

name: demo
version: 0

common all
    default-language: Haskell2010
    hs-source-dirs: .
    build-depends: base

library

library a
    import: all
    visibility: public
    build-depends:
      demo:b,
      fooa ^>= 4.13

library b
    import: all
    visibility: public
    build-depends:
      foob ^>= 4.13

library c
    import: all
    visibility: public
    build-depends:
      fooc ^>= 4.13
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fooa") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "foob") (Just CoreDependency),
                    BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fooc") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles sub libraries" $ do
        let inputRequirements =
              [r|cabal-version: 3.0

name: demo
version: 0
library

foreign-library myforeignlib
  type:                native-shared
  lib-version-info:    6:3:2

  if os(Windows)
    options: standalone
    mod-def-file: MyForeignLib.def

  other-modules:       MyForeignLib.SomeModule
                       MyForeignLib.SomeOtherModule
  build-depends:       base >=4.7 && <4.9, fooa
  hs-source-dirs:      src
  c-sources:           csrc/MyForeignLibWrapper.c
  default-language:    Haskell2010
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "fooa") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let inputRequirements =
              [r|cabal-version: 2.4
name: check-run-creator
version: 0.0.1
library
|]
            input = HaskellCabalInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected
