module Effect.DetermineDependencies.Backend.LanguageFiles.Rust.CargoManifestSpec (spec) where

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
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Rust.CargoManifest
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

{-
[dependencies]
time = "0.1.12"
some-crate = { version = "1.0", registry = "my-registry" }
rand = { git = "https://github.com/rust-lang-nursery/rand" }
other = { git = "https://github.com/rust-lang-nursery/rand", branch = "next" }
hello_utils = { path = "hello_utils" }

[dev-dependencies]
tempdir = "0.3"

[build-dependencies]
cc = "1.0.3"
-}

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a Cargo Manifest" $ do
      it "determines a simple named dependencies" $ do
        let input =
              [r|
[dependencies]
e682b37 = "0.5.0"
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ CargoManifestInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "e682b37") (Just CoreDependency),
                      BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "blah") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "strips quotes from named dependencies" $ do
        let input =
              [r|
[dependencies]
"e682b37" = "0.5.0"
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ CargoManifestInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "e682b37") (Just CoreDependency),
                      BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "blah") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "determines dev dependencies correctly" $ do
        let input =
              [r|
[dev-dependencies]
records = ">0.5.0"
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ CargoManifestInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "records") (Just DevDependency),
                      BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "blah") (Just DevDependency)
                    ]

        result `shouldBe` expected

      it "determines build dependencies correctly" $ do
        let input =
              [r|
[build-dependencies]
records = ">0.5.0"
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ CargoManifestInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "records") (Just DevDependency),
                      BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "blah") (Just DevDependency)
                    ]

        result `shouldBe` expected

      it "determines qualified repo dependencies" $ do
        let input =
              [r|
[dependencies]
some-crate = { version = "1.0", registry = "my-registry" }
rand = { git = "https://github.com/rust-lang-nursery/rand" }
other = { git = "https://github.com/rust-lang-nursery/other", branch = "next" }
hello_utils = { path = "hello_utils" }
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ CargoManifestInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "some-crate") (Just CoreDependency),
                      BasicDependency Rust (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "rust-lang-nursery") (RepoName "rand")) (Just $ DependencyName "rand")) (Just CoreDependency),
                      BasicDependency Rust (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "rust-lang-nursery") (RepoName "other")) (Just $ DependencyName "other")) (Just CoreDependency),
                      BasicDependency Rust (DependencyIdentifierNamed $ DependencyName "hello_utils") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "handles other repo hosts dependencies" $ do
        let input =
              [r|
[dependencies]
rand = { git = "https://bitbucket.org/rust-lang-nursery/rand" }
other = { git = "https://gitlab.com/rust-lang-nursery/other", branch = "next" }
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ CargoManifestInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Rust (DependencyIdentifierRepo (QualifiedRepo Bitbucket (RepoOwner "rust-lang-nursery") (RepoName "rand")) (Just $ DependencyName "rand")) (Just CoreDependency),
                      BasicDependency Rust (DependencyIdentifierRepo (QualifiedRepo GitLab (RepoOwner "rust-lang-nursery") (RepoName "other")) (Just $ DependencyName "other")) (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let result = determineDependencies (GitPath "path") $ CargoManifestInput ""
            expected =
              Right V.empty

        result `shouldBe` expected
