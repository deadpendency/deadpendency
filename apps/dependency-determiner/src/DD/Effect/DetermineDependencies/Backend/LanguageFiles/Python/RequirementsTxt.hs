module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.RequirementsTxt
  ( RequirementsTxtInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileType
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

newtype RequirementsTxt = RequirementsTxt
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype RequirementsTxtInput = RequirementsTxtInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

parserRequirementsTxt :: MParser RequirementsTxt
parserRequirementsTxt =
  parserDependencies <&> RequirementsTxt

parserDependencies :: MParser (V.Vector BasicDependency)
parserDependencies = do
  -- -r is a python requirements file include so we ignore
  maybeFirstDepAsVector <- M.optional $ M.try $ M.notFollowedBy (M.string "-r ") *> parserDependency
  parserUntilDepOrEnd
  nestedVectorDeps <- M.many $ ML.lexeme parserUntilDepOrEnd parserDependency
  let deps = V.concat nestedVectorDeps
      withFirstDep =
        case maybeFirstDepAsVector of
          Just firstDepAsVector -> firstDepAsVector V.++ deps
          Nothing -> deps
  pure withFirstDep

parserUntilDepOrEnd :: MParser ()
parserUntilDepOrEnd =
  M.skipManyTill
    M.anySingle
    (parserEOLWithDepNext <|> M.eof)

parserEOLWithDepNext :: MParser () -- -r is a python requirements file include so we ignore
parserEOLWithDepNext = void (M.try $ M.eol *> (M.notFollowedBy (M.string "-r ") *> M.lookAhead depNameChar))

parserDependency :: MParser (V.Vector BasicDependency)
parserDependency =
  (parserDependencySkips $> V.empty)
    <|> (M.try parserGitDependency <&> V.singleton)
    <|> (parserSimpleDependency <&> V.singleton)

parserGitDependency :: MParser BasicDependency
parserGitDependency = do
  qualifiedRepo <- parserCoreQualifiedRepo
  maybeDependencyName <- M.optional $ M.try parserEgg
  pure $
    BasicDependency
      Python
      ( DependencyIdentifierRepo
          qualifiedRepo
          maybeDependencyName
      )
      Nothing

parserEgg :: MParser DependencyName
parserEgg = do
  _ <- M.string "#egg="
  M.some depNameChar <&> DependencyName . pack

parserSimpleDependency :: MParser BasicDependency
parserSimpleDependency = M.some depNameChar <&> nameToDependency

parserDependencySkips :: MParser ()
parserDependencySkips = void $ M.try $ M.string "python"

nameToDependency :: String -> BasicDependency
nameToDependency name = BasicDependency Python (DependencyIdentifierNamed $ DependencyName $ pack name) Nothing

instance CanDetermineDependencies RequirementsTxtInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile PipRequirementsTxt gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserRequirementsTxt "requirements.txt"
      . _pjText
