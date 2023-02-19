module Effect.DetermineDependencies.Backend.LanguageFiles.JavaScript.PackageJsonSpec (spec) where

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
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.JavaScript.PackageJson
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a package.json" $ do
      it "determines happy day dependencies" $ do
        let dependencies = Object $ KM.fromList [("cool-lib", "1.2.3"), ("other-lib", "3.4.5")]
            devDependencies = Object $ KM.fromList [("test-lib", "7.8.9")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "dependencies" .= dependencies,
                  "devDependencies" .= devDependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "cool-lib") (Just CoreDependency),
                    BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "other-lib") (Just CoreDependency),
                    BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "test-lib") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles weird test chars" $ do
        let dependencies = Object $ KM.fromList [("cool-lib", "1.2.3")]
            devDependencies = Object $ KM.fromList [("@testing-library/jest-dom", "7.8.9")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "dependencies" .= dependencies,
                  "devDependencies" .= devDependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "cool-lib") (Just CoreDependency),
                    BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "@testing-library/jest-dom") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "determines dependencies pointing at github" $ do
        let dependencies =
              Object $
                KM.fromList
                  [ ("dep1", "git+ssh://git@github.com:npm/cli.git#v1.0.27"),
                    ("dep2", "git+https://isaacs@github.com/npm/cli2.git"),
                    ("dep3", "expressjs/express"),
                    ("dep4", "mochajs/mocha#4727d357ea")
                  ]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "dependencies" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = sortV <$> determineDependencies (GitPath "path") input
            expected =
              Right $
                sortV $
                  V.fromList
                    [ BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "npm") (RepoName "cli")) (Just $ DependencyName "dep1")) (Just CoreDependency),
                      BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "expressjs") (RepoName "express")) (Just $ DependencyName "dep3")) (Just CoreDependency),
                      BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "mochajs") (RepoName "mocha")) (Just $ DependencyName "dep4")) (Just CoreDependency),
                      BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "npm") (RepoName "cli2")) (Just $ DependencyName "dep2")) (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "other repos are gracefully handled" $ do
        let dependencies =
              Object $
                KM.fromList
                  [ ("dep1", "git+ssh://git@bitbucket.org:npm/cli.git#v1.0.27"),
                    ("dep2", "git+https://gitlab.com/npm/cli2.git")
                  ]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "dependencies" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = sortV <$> determineDependencies (GitPath "path") input
            expected =
              Right $
                sortV $
                  V.fromList
                    [ BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo Bitbucket (RepoOwner "npm") (RepoName "cli")) (Just $ DependencyName "dep1")) (Just CoreDependency),
                      BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo GitLab (RepoOwner "npm") (RepoName "cli2")) (Just $ DependencyName "dep2")) (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "handles no dev dependencies" $ do
        let dependencies = Object $ KM.fromList [("cool-lib", "1.2.3"), ("other-lib", "3.4.5")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "dependencies" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "cool-lib") (Just CoreDependency),
                    BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "other-lib") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "loads optional dependencies" $ do
        let dependencies = Object $ KM.fromList [("cool-lib", "1.2.3"), ("other-lib", "3.4.5")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "optionalDependencies" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "cool-lib") (Just CoreDependency),
                    BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "other-lib") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let devDependencies = Object $ KM.fromList [("test-lib", "7.8.9")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "devDependencies" .= devDependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "test-lib") (Just DevDependency))

        result `shouldBe` expected

      it "handles no dependencies at all" $ do
        let inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "handles dependencies with object values" $ do
        let devDependencies = Object $ KM.fromList [("test-lib", Object $ KM.fromList [("some", "key")])]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "devDependencies" .= devDependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = PackageJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "test-lib") (Just DevDependency))

        result `shouldBe` expected

      xit "play area" $ do
        let rawJson =
              [r|
{
  "dependencies": {
    "@hapi/joi": "^17.1.0",
    "bcrypt": "^3.0.6",
    "cors": "^2.8.5",
    "dotenv": "^8.2.0",
    "express": "^4.17.1",
    "helmet": "^3.23.2",
    "jsonwebtoken": "^8.5.1",
    "knex": "^0.19.5",
    "knex-cleaner": "^1.3.0",
    "pg": "^7.12.1"
  },
  "devDependencies": {
    "cross-env": "^7.0.0",
    "jest": "^25.1.0",
    "nodemon": "^1.19.4",
    "supertest": "^4.0.2"
  }
}
|]
            input = PackageJsonInput rawJson
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "test-lib") (Just DevDependency))

        result `shouldBe` expected
