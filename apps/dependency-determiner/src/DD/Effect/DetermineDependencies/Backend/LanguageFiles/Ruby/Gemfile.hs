module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Ruby.Gemfile
  ( GemfileInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.QualifiedRepo
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Common.Parsing.RepoParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as ML

newtype Gemfile = Gemfile
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype GemfileInput = GemfileInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

{-
source 'https://rubygems.org'
gem 'image_processing',           '1.9.3'
gem 'database_cleaner', github: 'bmabey/database_cleaner'
gem 'rack', git: 'https://github.com/rack/rack'
group :development, :test do
  gem 'sqlite3', '1.4.2'
end
group :production do
  gem 'pg',         '1.2.3'
end
source 'https://rubygems.org' do
  gem 'rubocop'
end
gem 'rubocop', path: './some-path'
-}

parserGemfile :: MParser Gemfile
parserGemfile =
  parserDependencies <&> Gemfile

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

depBeginning :: MParser ()
depBeginning =
  (spaceTab *> void (M.string "gem") *> M.hspace1) <* M.notFollowedBy (untilLineFileEnd (M.string "path:"))

untilLineFileEnd :: MParser a -> MParser a
untilLineFileEnd parser =
  M.skipManyTill M.anySingle (M.eitherP endOfLineOrFile parser) >>= \case
    Right a -> pure a
    Left _ -> fail "parser not matched until end of line"

-- gem 'database_cleaner', github: 'bmabey/database_cleaner'
parserGem :: MParser BasicDependency
parserGem = do
  spaceTab
  M.string "gem"
  M.hspace1
  depName <- M.between quote quote (M.some (depNameChar <|> M.char '.'))
  maybeRepo <- rightToMaybe <$> M.skipManyTill M.anySingle (M.eitherP (M.lookAhead endOfLineOrFile) parserInlineRepo)
  pure $
    case maybeRepo of
      Just repo -> nameRepoToDependency depName repo
      Nothing -> nameToDependency depName

endOfLineOrFile :: MParser ()
endOfLineOrFile = void M.eol <|> M.eof

quote :: MParser Char
quote = M.char '\'' <|> M.char '\"' <|> M.char '`'

parserInlineRepo :: MParser QualifiedRepo
parserInlineRepo = M.try parserSimpleInlineRepo <|> M.try parserFullInlineRepo

parserFullInlineRepo :: MParser QualifiedRepo
parserFullInlineRepo = do
  M.string "git:"
  M.hspace
  M.between quote quote parserQualifiedRepo

parserSimpleInlineRepo :: MParser QualifiedRepo
parserSimpleInlineRepo = do
  M.string "github:"
  M.hspace
  M.between quote quote parserSimpleRepo

nameToDependency :: String -> BasicDependency
nameToDependency name = BasicDependency Ruby (DependencyIdentifierNamed $ DependencyName $ pack name) Nothing

nameRepoToDependency :: String -> QualifiedRepo -> BasicDependency
nameRepoToDependency name repo = BasicDependency Ruby (DependencyIdentifierRepo repo (Just $ DependencyName $ pack name)) Nothing

instance CanDetermineDependencies GemfileInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile BundlerGemfile gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserGemfile "Gemfile"
      . _pjText
