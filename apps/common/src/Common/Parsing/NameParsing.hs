module Common.Parsing.NameParsing
  ( depNameChar,
    depNameExpandedChar,
    takeDepNameChars,
    parserDependencyName,
    parserDependencyNamed,
  )
where

import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Parsing.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

depNameChar :: MParser Char
depNameChar = M.alphaNumChar <|> M.char '-' <|> M.char '_'

depNameExpandedChar :: MParser Char
depNameExpandedChar = depNameChar <|> M.char '@' <|> M.char '/' <|> M.char '.'

takeDepNameChars :: Text -> Text
takeDepNameChars input =
  fromMaybe "" (mParseMaybe (pack <$> M.some depNameChar) input)

parserDependencyName :: MParser DependencyName
parserDependencyName = DependencyName . pack <$> M.some depNameExpandedChar

parserDependencyNamed :: MParser DependencyIdentifier
parserDependencyNamed = DependencyIdentifierNamed <$> parserDependencyName
