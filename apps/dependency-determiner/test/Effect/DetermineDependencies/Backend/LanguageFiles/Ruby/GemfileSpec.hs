module Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.GemfileSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.Gemfile
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a Gemfile" $ do
      it "determines happy day dependencies" $ do
        let inputGemfile =
              [r|
source 'https://rubygems.org'

gem 'image_processing',           '1.9.3'
# woo
gem 'database_cleaner', github: 'bmabey/database_cleaner'
gem 'rack', git: 'https://github.com/rack/rack'
gem 'blah', git: 'https://randomgithost.com/blah/blah'

group :development, :test do
  gem 'sqlite3', '1.4.2'
end

group :production do
  gem 'pg',         '1.2.3'
end

source 'https://rubygems.org' do
  gem 'rubocop'
end
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "image_processing") Nothing,
                    BasicDependency Ruby (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "bmabey") (RepoName "database_cleaner")) (Just $ DependencyName "database_cleaner")) Nothing,
                    BasicDependency Ruby (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "rack") (RepoName "rack")) (Just $ DependencyName "rack")) Nothing,
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "blah") Nothing,
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "sqlite3") Nothing,
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "pg") Nothing,
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "rubocop") Nothing
                  ]

        result `shouldBe` expected

      it "ignores path dependencies" $ do
        let inputGemfile =
              [r|
source 'https://rubygems.org'

gem 'image_processing',           '1.9.3'
# woo
gem 'rubocop', path: './some-path'
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "image_processing") Nothing
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let inputGemfile =
              [r|
source 'https://rubygems.org'
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "will ignore gemspec line" $ do
        let inputGemfile =
              [r|
source 'https://rubygems.org'

gemspec

gem "turbolinks", "~> 5"
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "turbolinks") Nothing
                  ]

        result `shouldBe` expected

      it "handles many gems without linebreaks in between" $ do
        let inputGemfile =
              [r|
source 'https://rubygems.org'

gem "turbolinks", "~> 5"

gem "webpacker", "~> 4.0", require: false
gem "bcrypt", "~> 3.1.11", require: false
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "turbolinks") Nothing,
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "webpacker") Nothing,
                    BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "bcrypt") Nothing
                  ]

        result `shouldBe` expected

      it "ignores lines with gem in comments" $ do
        let inputGemfile =
              [r|
source 'https://rubygems.org'

platforms :rbx do
  # The rubysl-yaml gem doesn't ship with Psych by default as it needs
  # libyaml that isn't always available.
  gem "psych", "~> 3.0"
end
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "psych") Nothing
                  ]

        result `shouldBe` expected

      it "handles tabs" $ do
        let inputGemfile =
              [r|
group :development do
		gem 'rubocop', require: false
end
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "rubocop") Nothing
                  ]

        result `shouldBe` expected

      it "handles . in gem name" $ do
        let inputGemfile =
              [r|
group :development do
	gem 'promise.rb'
end
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "promise.rb") Nothing
                  ]

        result `shouldBe` expected

      it "handles backticks" $ do
        let inputGemfile =
              [r|
source `https://rubygems.org`
gem `github-pages`
|]
            input = GemfileInput inputGemfile
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName "github-pages") Nothing
                  ]

        result `shouldBe` expected
