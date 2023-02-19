module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Scala.BuildSbt
  ( BuildSbtInput (..),
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

newtype BuildSbt = BuildSbt
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype BuildSbtInput = BuildSbtInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

{-
lazy val root = (project in file(".")).
  settings(
    name := "entry",
    version := "1.0",
    scalaVersion := "2.12.8"
  )

// first percent can be a double percent
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.14"
libraryDependencies += "commons-io" % "commons-io" % "2.6" % "test"

val circeVersion = "0.11.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic"
).map(_ % circeVersion)

libraryDependencies ++=
  Seq(
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.4",
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
    "org.foobar" %% "foobar" % "1.8"
    )
-}

parserBuildSbt :: MParser BuildSbt
parserBuildSbt =
  parserDependencies <&> BuildSbt

parserDependencies :: MParser (V.Vector BasicDependency)
parserDependencies = do
  parserUntilDepOrEnd
  deps <- M.many $ ML.lexeme parserUntilDepOrEnd parserDependencyBlock
  pure $ V.concat deps

parserUntilDepOrEnd :: MParser ()
parserUntilDepOrEnd = do
  M.skipManyTill
    M.anySingle
    (parserEOLWithDepNext <|> M.eof)

parserEOLWithDepNext :: MParser ()
parserEOLWithDepNext = void (M.try $ M.eol *> M.lookAhead depBeginning)

depBeginning :: MParser Text
depBeginning =
  M.hspace *> M.string "libraryDependencies +"

parserDependencyBlock :: MParser (V.Vector BasicDependency)
parserDependencyBlock =
  M.try parserDependencyList <|> (fromMaybeV <$> parserSingleDependency)

-- libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.14"
-- libraryDependencies += "commons-io" %% "commons-io" % "2.6" % "test"
parserSingleDependency :: MParser (Maybe BasicDependency)
parserSingleDependency = do
  M.hspace
  M.string "libraryDependencies +="
  basicDep <- parseMaybeDependencyCore
  M.skipManyTill M.anySingle (M.lookAhead endOfLineOrFile)
  pure basicDep

parseMaybeDependencyCore :: MParser (Maybe BasicDependency)
parseMaybeDependencyCore =
  Just <$> M.try parserDependencyCore <|> (parserVariableDependency $> Nothing)

-- libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
parserDependencyCore :: MParser BasicDependency
parserDependencyCore = do
  M.space
  namespace <- pack <$> M.between (M.char '"') (M.char '"') (M.some depNameExpandedChar)
  M.hspace
  M.char '%' *> M.optional (M.char '%')
  M.hspace
  M.char '"'
  -- The dependency name can include a scala version but we don't want that eg. scalatest_2.10
  name <- pack <$> M.someTill depNameChar (M.optional scalaVersionsSuffix *> M.char '"')
  pure $
    nameToDependency namespace name

parserVariableDependency :: MParser ()
parserVariableDependency = do
  M.space
  M.some M.alphaNumChar
  pure ()

scalaVersionsSuffix :: MParser Text
scalaVersionsSuffix =
  M.char '_'
    *> ( M.string "2.10"
           <|> M.string "2.11"
           <|> M.string "2.12"
           <|> M.string "2.10"
           <|> M.string "3"
       )

{-
libraryDependencies ++=
  Seq(
    "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.4",
    "org.scalatest" % "scalatest" % "1.9.1" % "test",
    "org.foobar" %% "foobar" % "1.8"
    )
-}
parserDependencyList :: MParser (V.Vector BasicDependency)
parserDependencyList = do
  M.hspace
  M.string "libraryDependencies ++="
  M.space
  M.string "Seq("
  M.space
  basicDeps <- concatMaybeV . V.fromList <$> M.some (M.try (parseMaybeDependencyCore <* betweenDeps))
  M.skipManyTill M.anySingle (M.char ')')
  pure basicDeps
  where
    betweenDeps :: MParser ()
    betweenDeps = do
      M.space
      void $ M.optional $ M.try $ M.skipManyTill M.anySingle (M.char ',')

endOfLineOrFile :: MParser ()
endOfLineOrFile = void M.eol <|> M.eof

nameToDependency :: Text -> Text -> BasicDependency
nameToDependency namespace name =
  BasicDependency
    Scala
    (DependencyIdentifierNamed (DependencyName $ namespace <> "/" <> name))
    Nothing

instance CanDetermineDependencies BuildSbtInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile DFT.BuildSbt gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserBuildSbt "BuildSbt"
      . _pjText
