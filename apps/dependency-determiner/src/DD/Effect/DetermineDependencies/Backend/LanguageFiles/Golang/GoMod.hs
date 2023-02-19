module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Golang.GoMod
  ( GoModInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileType qualified as DFT
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as ML

newtype GoMod = GoMod
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype GoModInput = GoModInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

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

parserGoMod :: MParser GoMod
parserGoMod =
  parserDependencies <&> GoMod

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

parserAllDependencyList :: MParser (V.Vector BasicDependency)
parserAllDependencyList =
  M.try (fromMaybeV <$> parserSingleDependency)
    <|> parserSingleDependencyList

-- require (
depBeginning :: MParser ()
depBeginning = void $ do
  M.string "require"
  M.hspace
  void $ M.optional $ M.char '('

{-
require golang.org/x/text v0.3.2
-}
parserSingleDependency :: MParser (Maybe BasicDependency)
parserSingleDependency = do
  _ <- M.string "require"
  parserSkippedDependency

{-
require (
	github.com/alecthomas/template v0.0.0-20160405071501-a0175ee3bccc // indirect
	github.com/alecthomas/units v0.0.0-20151022065526-2efee857e7cf // indirect
	github.com/gorilla/mux v1.6.2
	github.com/sirupsen/logrus v1.2.0
	gopkg.in/alecthomas/kingpin.v2 v2.2.6 // indirect
)
-}
parserSingleDependencyList :: MParser (V.Vector BasicDependency)
parserSingleDependencyList = do
  M.string "require"
  M.hspace
  M.char '('
  M.space
  parserDependencyList

{-
	github.com/gorilla/mux v1.6.2
	github.com/sirupsen/logrus v1.2.0
-}
parserDependencyList :: MParser (V.Vector BasicDependency)
parserDependencyList = do
  M.space
  maybeDeps <- M.many $ M.try (parserSkippedDependency <* M.skipManyTill M.anySingle (void M.eol <|> M.eof))
  pure $ concatMaybeV $ V.fromList maybeDeps

parserSkippedDependency :: MParser (Maybe BasicDependency)
parserSkippedDependency =
  M.try parserIndirectDep
    <|> M.try parserStdLibDep
    <|> (Just <$> parserDependency)

parserIndirectDep :: MParser (Maybe BasicDependency)
parserIndirectDep = do
  M.skipManyTill (M.anySingleBut '\n') (M.string "//" *> M.hspace *> M.string "indirect")
  pure Nothing

parserStdLibDep :: MParser (Maybe BasicDependency)
parserStdLibDep = do
  M.hspace
  M.someTill depNameChar (M.char '/')
  pure Nothing

parserDependency :: MParser BasicDependency
parserDependency = do
  M.hspace
  dependencyName <- parserDependencyName
  pure $ dependencyNameToDependency dependencyName

dependencyNameToDependency :: DependencyName -> BasicDependency
dependencyNameToDependency dependencyName = BasicDependency Golang (DependencyIdentifierNamed dependencyName) Nothing

instance CanDetermineDependencies GoModInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile DFT.GoMod gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserGoMod "GoMod"
      . _pjText
