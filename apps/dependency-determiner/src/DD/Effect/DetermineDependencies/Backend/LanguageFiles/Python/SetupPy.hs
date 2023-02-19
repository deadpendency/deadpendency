module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.SetupPy
  ( SetupPyInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileType qualified as DFT
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Common.Parsing.RepoParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as ML

newtype SetupPy = SetupPy
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype SetupPyInput = SetupPyInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

{-
install_requires=[
    'PyYAML',
    'pandas==0.23.3',
    'numpy>=1.14.5'
]

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

parserSetupPy :: MParser SetupPy
parserSetupPy =
  parserDependencies <&> SetupPy

parserDependencies :: MParser (V.Vector BasicDependency)
parserDependencies = do
  parserUntilDepOrEnd
  deps <- M.many $ ML.lexeme parserUntilDepOrEnd parserAllDependencyList
  pure $ V.concat deps

parserUntilDepOrEnd :: MParser ()
parserUntilDepOrEnd = do
  M.skipManyTill
    M.anySingle
    (parserEOLWithDepNext <|> M.eof)

parserEOLWithDepNext :: MParser ()
parserEOLWithDepNext = void (M.try $ M.eol *> M.lookAhead depBeginning)

depBeginning :: MParser ()
depBeginning = M.try simpleDepBeginning <|> extrasDepBeginning

parserAllDependencyList :: MParser (V.Vector BasicDependency)
parserAllDependencyList = M.try parserSingleDependencyList <|> parserExtrasDependencyList

-- simple case with a list of deps as a string

simpleDepBeginning :: MParser ()
simpleDepBeginning = void $ do
  M.hspace
  simpleDepKeys
  M.hspace
  M.char '='
  M.hspace
  M.lookAhead $ M.char '['

simpleDepKeys :: MParser Text
simpleDepKeys =
  M.string' "install_requires"
    <|> M.string' "requirements"
    <|> M.string' "setup_requires"
    <|> M.string' "tests_require"

{-
  install_requires = [
      'PyYAML',
      'pandas==0.23.3',
      'numpy>=1.14.5'
  ]
-}
parserSingleDependencyList :: MParser (V.Vector BasicDependency)
parserSingleDependencyList = do
  simpleDepBeginning
  M.space
  parserDependencyList

-- extras complex case, which is an object, with key values, and the values are list of deps

{-
  extras_require={
    'plotting': [
      'matplotlib>=2.2.0',
      'jupyter'
      ],
    'other': [
      'woo>=2.2.0',
      'moredep'
      ]

extras_require = { 'plotting': ['matplotlib>=2.2.0', 'jupyter'], 'other': ['matplotlib>=2.2.0', 'jupyter'] }
-}
extrasDepBeginning :: MParser ()
extrasDepBeginning = void $ do
  M.space
  M.string' "extras_require"
  M.hspace
  M.char '='
  M.hspace
  M.char '{'

parserExtrasDependencyList :: MParser (V.Vector BasicDependency)
parserExtrasDependencyList = do
  extrasDepBeginning
  M.space
  namedDepLists <- M.many (M.try parserExtrasNamedDependencyList)
  pure $ V.concat namedDepLists

parserExtrasNamedDependencyList :: MParser (V.Vector BasicDependency)
parserExtrasNamedDependencyList = do
  _ <- M.skipManyTill M.anySingle (M.lookAhead (M.char '['))
  parserDependencyList

{-
  [
  'matplotlib>=2.2.0',
  'jupyter'
  ]
-}
parserDependencyList :: MParser (V.Vector BasicDependency)
parserDependencyList = do
  M.hspace
  M.char '['
  M.space
  deps <- M.many $ M.try parserDependency
  M.skipManyTill (M.anySingleBut '[') (M.char ']')
  M.hspace
  M.optional $ M.char ','
  pure $ V.fromList deps

{-
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
-}
-- A known issue is it will not be correct if the comments have quotes in them
parserDependency :: MParser BasicDependency
parserDependency = do
  M.skipManyTill (M.anySingleBut ']') quote
  dependency <- M.try parserGitDependency <|> parserNamedDependency
  M.skipManyTill (M.anySingleBut '\n') quote
  pure dependency

parserNamedDependency :: MParser BasicDependency
parserNamedDependency = parserPyDependencyName <&> nameToDependency

parserPyDependencyName :: MParser String
parserPyDependencyName = M.some (depNameChar <|> M.char '.')

parserGitDependency :: MParser BasicDependency
parserGitDependency = do
  dependencyName <- pack <$> parserPyDependencyName
  M.hspace
  M.char '@'
  M.hspace
  qualifiedRepo <- parserCoreQualifiedRepo
  pure $
    BasicDependency
      Python
      ( DependencyIdentifierRepo
          qualifiedRepo
          (Just $ DependencyName dependencyName)
      )
      Nothing

quote :: MParser Char
quote = M.char '\'' <|> M.char '\"'

nameToDependency :: String -> BasicDependency
nameToDependency name = BasicDependency Python (DependencyIdentifierNamed $ DependencyName $ pack name) Nothing

instance CanDetermineDependencies SetupPyInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile DFT.PythonSetupPy gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserSetupPy "SetupPy"
      . _pjText
