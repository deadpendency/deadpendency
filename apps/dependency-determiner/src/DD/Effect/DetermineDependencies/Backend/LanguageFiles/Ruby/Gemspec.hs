module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.Gemspec
  ( GemspecInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as ML

newtype Gemspec = Gemspec
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype GemspecInput = GemspecInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

{-
\$LOAD_PATH.unshift File.expand_path('lib', __dir__)
require 'rubocop/version'
Gem::Specification.new do |s|
  s.add_runtime_dependency('parallel', '~> 1.10')
  s.add_runtime_dependency('parser', '>= 2.7.0.1')
  s.add_runtime_dependency('rainbow', '>= 2.2.2', '< 4.0')
  s.add_development_dependency('bundler', '>= 1.15.0', '< 3.0')
end
-}

parserGemspec :: MParser Gemspec
parserGemspec =
  parserDependencies <&> Gemspec

parserDependencies :: MParser (V.Vector BasicDependency)
parserDependencies = do
  parserUntilDepOrEnd
  deps <- M.many $ ML.lexeme parserUntilDepOrEnd parserGem
  pure $ V.fromList deps

parserUntilDepOrEnd :: MParser ()
parserUntilDepOrEnd =
  M.skipManyTill
    M.anySingle
    (parserEOLWithDepNext <|> M.eof)

parserEOLWithDepNext :: MParser ()
parserEOLWithDepNext = void $ M.try $ M.some M.eol *> M.lookAhead depBeginning

depBeginning :: MParser Text
depBeginning = do
  preceedingDep
  M.string "add_"
  M.string "runtime" <|> M.string "development"
  M.string "_dependency("

preceedingDep :: MParser Char
preceedingDep = M.hspace *> M.some M.alphaNumChar *> M.char '.'

parserGem :: MParser BasicDependency
parserGem = do
  _ <- preceedingDep
  M.try parserCoreDep <|> parserDevDep

parserCoreDep :: MParser BasicDependency
parserCoreDep = do
  M.string "add_runtime_dependency("
  depName <- M.between quote quote (M.some depNameChar)
  pure $
    nameToDependency depName CoreDependency

parserDevDep :: MParser BasicDependency
parserDevDep = do
  M.string "add_development_dependency("
  depName <- M.between quote quote (M.some depNameChar)
  pure $
    nameToDependency depName DevDependency

quote :: MParser Char
quote = M.char '\'' <|> M.char '\"'

nameToDependency :: String -> DependencyType -> BasicDependency
nameToDependency name depType = BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName $ pack name) (Just depType)

instance CanDetermineDependencies GemspecInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile RubyGemsGemspec gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserGemspec "Gemspec"
      . _pjText
