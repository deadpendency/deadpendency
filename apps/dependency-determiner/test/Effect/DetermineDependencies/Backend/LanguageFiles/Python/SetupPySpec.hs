module Effect.DetermineDependencies.Backend.LanguageFiles.Python.SetupPySpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.SetupPy
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

{-
setup(
    name='example',
    version='0.1.0',
    description='Setting up a python package',
    install_requires=[
        'PyYAML',
        'pandas==0.23.3',
        'numpy>=1.14.5'
    ],
    extras_require={'plotting': ['matplotlib>=2.2.0', 'jupyter']},
    setup_requires=['pytest-runner', 'flake8'],
    tests_require=['pytest']
)
-}

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a SetupPy" $ do
      it "loads happy day test" $ do
        let inputRequirements =
              [r|
setup_requires= ["python-dateutil>=2.4","text-unicode==1.3"]

tests_require=[
    "blah;2.4",
    "cooldep==1.3",
]

setup(
    name='Faker',
    install_requires=[
        'PyYAML',
        'pandas==0.23.3',
        'numpy>=1.14.5'
    ],
    extras_require={'plotting': ['matplotlib>=2.2.0', 'jupyter']},
    setup_requires=setup_requires,
    tests_require=tests_require
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "blah") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "cooldep") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "PyYAML") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "pandas") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "numpy") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "matplotlib") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "jupyter") Nothing
                  ]

        result `shouldBe` expected

      it "loads install_requires deps" $ do
        let inputRequirements =
              [r|
setup(
    name='Faker',
    install_requires=[
        "python-dateutil>=2.4",
        "text-unicode==1.3",
    ],
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing
                  ]

        result `shouldBe` expected

      it "loads extras_require deps" $ do
        let inputRequirements =
              [r|
setup(
    name='Faker',
    extras_require={
      'plotting': [
        'matplotlib>=2.2.0',
        'jupyter'
        ],
        'other': [
          'woo>=2.2.0',
          'moredep'
          ],
    },
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "matplotlib") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "jupyter") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "woo") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "moredep") Nothing
                  ]

        result `shouldBe` expected

      it "loads setup_requires deps" $ do
        let inputRequirements =
              [r|
setup(
    name='Faker',
    setup_requires=[
        "python-dateutil>=2.4",
        "text-unicode==1.3",
    ],
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing
                  ]

        result `shouldBe` expected

      it "loads tests_require deps" $ do
        let inputRequirements =
              [r|
setup(
    name='Faker',
    tests_require=[
        "python-dateutil>=2.4",
        "text-unicode==1.3",
    ],
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing
                  ]

        result `shouldBe` expected

      it "loads install_requires deps variable style" $ do
        let inputRequirements =
              [r|
install_requires=[
    "python-dateutil>=2.4",
    "text-unicode==1.3",
],

setup(
    name='Faker',
    install_requires=install_requires,
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing
                  ]

        result `shouldBe` expected

      it "loads requirements in deps variable style" $ do
        let inputRequirements =
              [r|
requirements=[
    "python-dateutil>=2.4",
    "text-unicode==1.3",
],

setup(
    name='Faker',
    install_requires=requirements,
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing
                  ]

        result `shouldBe` expected

      it "loads extras_require deps variable style" $ do
        let inputRequirements =
              [r|
extras_require = { 'plotting': ['matplotlib>=2.2.0', 'jupyter'], 'other': ['otherdep>=2.2.0', 'moredep'] }

setup(
    name='Faker',
    extras_require=extras_require,
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "matplotlib") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "jupyter") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "otherdep") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "moredep") Nothing
                  ]

        result `shouldBe` expected

      it "loads setup_requires deps variable style" $ do
        let inputRequirements =
              [r|
setup_requires= ["python-dateutil>=2.4","text-unicode.six==1.3"]

setup(
    name='Faker',
    setup_requires = setup_requires,
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode.six") Nothing
                  ]

        result `shouldBe` expected

      it "loads tests_require deps variable style" $ do
        let inputRequirements =
              [r|
tests_require=[
    "python-dateutil>=2.4",
    "text-unicode==1.3",
]

setup(
    name='Faker',
    tests_require=tests_require,
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "python-dateutil") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "text-unicode") Nothing
                  ]

        result `shouldBe` expected

      it "loads no deps" $ do
        let inputRequirements = ""
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected = Right V.empty
        result `shouldBe` expected

      it "gracefully handles comments in deps deps" $ do
        let inputRequirements =
              [r|
setup(
    name='Faker',
    install_requires=[
        ## dep list
## see below
        "one>=2.4", # awesome dep
        "two>=2.4",
        # these two could be removed?
        "three>=2.4",
        "four>=2.4"

        ## add some more deps?
    ],
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "one") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "two") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "three") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "four") Nothing
                  ]

        result `shouldBe` expected

      it "loads git deps" $ do
        let inputRequirements =
              [r|
setup(
    name='Faker',
    install_requires=[
        'ds_tools@ git+git://github.com/dskrypa/ds_tools',
        'pyutils @ git+ssh://git@github.com/vphpersson/pyutils.git#egg=pyutils'
        "onnxconverter-common@git+git://github.com/microsoft/onnxconverter-common.git@f64ca15989b6dc95a1f3507ff6e4c395ba12dff5#egg=onnxconverter-common",
        'xml_helpers@git+https://github.com/woah/yes/'
    ],
)
|]
            input = SetupPyInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "dskrypa") (RepoName "ds_tools")) (Just $ DependencyName "ds_tools")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "vphpersson") (RepoName "pyutils")) (Just $ DependencyName "pyutils")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "microsoft") (RepoName "onnxconverter-common")) (Just $ DependencyName "onnxconverter-common")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "woah") (RepoName "yes")) (Just $ DependencyName "xml_helpers")) Nothing
                  ]

        result `shouldBe` expected
