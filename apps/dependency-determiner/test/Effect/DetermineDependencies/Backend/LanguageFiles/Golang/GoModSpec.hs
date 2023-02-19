module Effect.DetermineDependencies.Backend.LanguageFiles.Golang.GoModSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Golang.GoMod
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

{-
module github.com/skyrocknroll/go-mod-example

require (
	github.com/alecthomas/template v0.0.0-20160405071501-a0175ee3bccc // indirect
	github.com/alecthomas/units v0.0.0-20151022065526-2efee857e7cf // indirect
	github.com/gorilla/mux v1.6.2
	github.com/sirupsen/logrus v1.2.0
	gopkg.in/alecthomas/kingpin.v2 v2.2.6 // indirect
)
-}

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a GoMod" $ do
      it "loads happy day test" $ do
        let inputRequirements =
              [r|
module github.com/simonireilly/go-modules-example

require (
	golang.org/x/text v0.3.2
	github.com/gorilla/mux v1.6.2
	github.com/sirupsen/logrus v1.2.0
)
|]
            input = GoModInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "golang.org/x/text") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/gorilla/mux") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/sirupsen/logrus") Nothing
                  ]

        result `shouldBe` expected

      it "loads single line style deps" $ do
        let inputRequirements =
              [r|
module github.com/simonireilly/go-modules-example

require golang.org/x/text v0.3.2
require github.com/gorilla/mux v0.3.2

require (
	github.com/sirupsen/logrus v1.2.0
)
|]
            input = GoModInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "golang.org/x/text") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/gorilla/mux") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/sirupsen/logrus") Nothing
                  ]

        result `shouldBe` expected

      it "ignores indirect dependencies" $ do
        let inputRequirements =
              [r|
module github.com/simonireilly/go-modules-example

require (
	golang.org/x/text v0.3.2  // indirect
	github.com/gorilla/mux v1.6.2
	github.com/sirupsen/logrus v1.2.0
)

require gopkg.in/yaml.v2 // indirect
|]
            input = GoModInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/gorilla/mux") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/sirupsen/logrus") Nothing
                  ]

        result `shouldBe` expected

      it "ignores std lib deps" $ do
        let inputRequirements =
              [r|
module github.com/simonireilly/go-modules-example

require (
	golang.org/x/text v0.3.2
	github.com/gorilla/mux v1.6.2
	github.com/sirupsen/logrus v1.2.0
  net/http/httptest
)
|]
            input = GoModInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "golang.org/x/text") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/gorilla/mux") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "github.com/sirupsen/logrus") Nothing
                  ]

        result `shouldBe` expected

      it "does not strip versions" $ do
        let inputRequirements =
              [r|
module github.com/simonireilly/go-modules-example

require (
	gopkg.in/go-playground/validator.v9
	rsc.io/quote/v3
  gopkg.in/yaml.v2
)
|]
            input = GoModInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "gopkg.in/go-playground/validator.v9") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "rsc.io/quote/v3") Nothing,
                    BasicDependency Golang (DependencyIdentifierNamed $ DependencyName "gopkg.in/yaml.v2") Nothing
                  ]

        result `shouldBe` expected
