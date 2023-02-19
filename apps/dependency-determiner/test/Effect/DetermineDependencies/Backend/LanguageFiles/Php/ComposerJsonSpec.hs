module Effect.DetermineDependencies.Backend.LanguageFiles.Php.ComposerJsonSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Php.ComposerJson
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import Test.Hspec

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a composer.json" $ do
      it "determines happy day dependencies" $ do
        let dependencies = Object $ KM.fromList [("cool/cool-lib", "1.2.3"), ("cool/other-lib", "3.4.5")]
            devDependencies = Object $ KM.fromList [("cool/test-lib", "7.8.9")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "require" .= dependencies,
                  "require-dev" .= devDependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = ComposerJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/cool-lib") (Just CoreDependency),
                    BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/other-lib") (Just CoreDependency),
                    BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/test-lib") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles no dev dependencies" $ do
        let dependencies = Object $ KM.fromList [("cool/cool-lib", "1.2.3"), ("cool/other-lib", "3.4.5")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "require" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = ComposerJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/cool-lib") (Just CoreDependency),
                    BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/other-lib") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let devDependencies = Object $ KM.fromList [("cool/test-lib", "7.8.9")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "require-dev" .= devDependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = ComposerJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.singleton (BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/test-lib") (Just DevDependency))

        result `shouldBe` expected

      it "deps with no '/' are gracefully ignored" $ do
        let dependencies = Object $ KM.fromList [("no-slash", "1.2.3"), ("cool/valid", "3.4.5")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "require" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = ComposerJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input

            expected =
              Right $
                V.fromList
                  [ BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/valid") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "the php package is filtered" $ do
        let dependencies = Object $ KM.fromList [("cool/cool-lib", "1.2.3"), ("php", "3.4.5")]
            inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "require" .= dependencies,
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = ComposerJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Php (DependencyIdentifierNamed $ DependencyName "cool/cool-lib") (Just CoreDependency)
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let inputJson =
              object
                [ "boringOtherKeyOne" .= (3 :: Int),
                  "boringOtherKeyTwo" .= ("woah" :: Text)
                ]
            input = ComposerJsonInput (decodeUtf8 $ encode inputJson)
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected
