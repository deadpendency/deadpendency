module Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.GemspecSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.Gemspec
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a Gemspec" $ do
      it "determines happy day dependencies" $ do
        let inputGemspec =
              [r|
$LOAD_PATH.unshift File.expand_path('lib', __dir__)
require 'rubocop/version'

Gem::Specification.new do |s|
  s.add_runtime_dependency('parallel', '~> 1.10')
  s.add_runtime_dependency('parser', '>= 2.7.0.1')
  s.add_runtime_dependency('rainbow', '>= 2.2.2', '< 4.0')

  s.add_development_dependency('bundler', '>= 1.15.0', '< 3.0')
end
|]
            input = GemspecInput inputGemspec
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "parallel") (Just CoreDependency),
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "parser") (Just CoreDependency),
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "rainbow") (Just CoreDependency),
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "bundler") (Just DevDependency)
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let inputGemspec =
              [r|
$LOAD_PATH.unshift File.expand_path('lib', __dir__)
require 'rubocop/version'
|]
            input = GemspecInput inputGemspec
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected
