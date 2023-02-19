{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fno-warn-orphans #-}

module RP.Effect.ReadConfig.Backend.RepoConfig () where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.Dependency.File.DependenciesFileLoadDetails
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitFileMatch (GitFileMatch (GitFileMatch))
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.RepoConfig.FileLoadPlan
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import Common.Model.RepoConfig.RepoConfig
import Common.Model.RepoConfig.Rules.RulesConfig
import Common.Parsing.HsYAML ()
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Common.Parsing.RepoParsing
import Data.Map.Strict qualified as C
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Data.YAML
import RP.Effect.ReadConfig.Backend.RulesConfig ()
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

{-
  additional-dependency-files:
    - path: other-dependencies.txt
      type: pip-requirements-txt
    - path: 'custom/CustomPipfile'
      type: pypa-pipfile
  additional-deps:
    javascript:
      - react
      - react-dom
      - name: stack
        repo: commercialhaskell/stack
        hosted-on: github # optional, defaults to 'github'
  disable-auto-file-load: false
  ignore-failures:
    javascript:
      - react-dom
  rules-config:
    no-recent-package-release:
      warn-at-months: 6
      fail-at-months: 9
    no-recent-commit:
      warn-at-months: 3
      fail-at-months: 6
    few-yearly-commits:
      warn-at-count: 6
      fail-at-count: 3
-}

instance FromYAML RepoConfig where
  parseYAML =
    withMap "RepoConfig" $ \m ->
      RepoConfig
        <$> m .:? "additional-deps" .!= V.empty
        <*> m .:? "ignore-failures" .!= IDSpecific V.empty
        <*> m .:? "additional-dependency-files" .!= V.empty
        <*> m .:? "disable-auto-file-load" .!= FileLoadEnabled
        <*> m .:? "rules-config" .!= defaultRulesConfig

instance {-# OVERLAPPING #-} FromYAML (V.Vector BasicDependency) where
  parseYAML =
    withMap "AdditionalDependencyMap" $ \m ->
      C.foldlWithKey'
        ( \deps k adValues ->
            (V.++) <$> deps <*> (parseYAML k >>= flip parseAdditionalDeps adValues)
        )
        (pure V.empty)
        m

instance (FromYAML a) => FromYAML (NV.NonEmptyVector a) where
  parseYAML =
    withSeq "NonEmptyVector" $ \case
      [] -> fail "Empty list not valid"
      (x : xs) -> traverse parseYAML (NV.consV x (V.fromList xs))

instance {-# OVERLAPPING #-} FromYAML IgnoreDependenciesConfig where
  parseYAML s@Sequence {} = IDAll <$> parseYAML s
  parseYAML s@Mapping {} = IDSpecific <$> parseYAML s
  parseYAML _ = fail "Unexpected type for ignore dependencies"

instance {-# OVERLAPPING #-} FromYAML (V.Vector IgnoreLanguageDependencies) where
  parseYAML =
    withMap "IgnoreLanguageDependencies" $ \m ->
      C.foldlWithKey'
        ( \deps k adValues ->
            V.snoc <$> deps <*> (parseYAML k >>= flip parseIgnoredLanguageDeps adValues)
        )
        (pure V.empty)
        m

instance FromYAML DependencyName where
  parseYAML = withStr "DependencyName" (parseMToParser "DependencyName" parserDependencyName)

instance FromYAML DependenciesFileLoad where
  parseYAML = withMap "DependenciesFileLoad" $
    \m ->
      DependenciesFileLoad
        <$> m
          .: "type"
        <*> m
          .: "path"

instance FromYAML DependenciesFileLoadDetails where
  parseYAML = withStr "DependenciesFileLoadDetails" $ \s -> getResult s
    where
      getResult inputString
        | Text.null inputString = fail "Empty additional-dependency-file"
        | Text.isInfixOf "**" inputString = fail "Globbing is not supported for additional-dependency-files, you can use wildcards in the filename though."
        | Text.isInfixOf "*" inputString = pure $ uncurry DFLDDirectorySearch $ getDirectoryAndFile inputString
        | otherwise = pure $ DFLDSpecific inputString

getDirectoryAndFile :: Text -> (GitPath, GitFileMatch)
getDirectoryAndFile input =
  let trimStart = stripPrefix "/" $ stripPrefix "./" input
      maybePathChunks = nonEmpty $ Text.splitOn "/" trimStart
   in case maybePathChunks of
        Nothing -> error "Should not be reachable"
        Just (fileName :| []) -> (GitPath "", GitFileMatch fileName)
        Just chunks -> (GitPath $ Text.intercalate "/" $ init chunks, GitFileMatch $ last chunks)

instance FromYAML DependenciesFileType where
  parseYAML =
    withStr "DependenciesFileType" $ \case
      "pip-requirements-txt" -> pure PipRequirementsTxt
      "python-setup-py" -> pure PythonSetupPy
      "pipenv-pipfile" -> pure PipenvPipfile
      "pyproject-toml" -> pure PyProjectToml
      "npm-package-json" -> pure NpmPackageJson
      "packagist-composer-json" -> pure PackagistComposerJson
      "bundler-gemfile" -> pure BundlerGemfile
      "rubygems-gemspec" -> pure RubyGemsGemspec
      "hpack-package-yaml" -> pure HpackPackageYaml
      "haskell-cabal" -> pure HaskellCabal
      "crates-cargo-toml" -> pure CratesCargoToml
      "dotnet-csharp-project" -> pure DotNetCSharpProject
      "dotnet-visualbasic-project" -> pure DotNetVisualBasicProject
      "maven-pom-xml" -> pure MavenPomXml
      "build-gradle" -> pure BuildGradle
      "build-sbt" -> pure BuildSbt
      "go-mod" -> pure GoMod
      unmatched -> fail ("Unexpected additional dependency file type: " <> unpack unmatched)

instance FromYAML FileLoadPlan where
  parseYAML (Scalar _ (SBool True)) = pure FileLoadDisabled
  parseYAML (Scalar _ (SBool False)) = pure FileLoadEnabled
  parseYAML s@Sequence {} = FileLoadDisabledForLangs . ordNubNV <$> parseYAML s
  parseYAML _ = fail "Unexpected type for disable-auto-file-load"

instance FromYAML ProgrammingLanguage where
  parseYAML =
    withStr
      "ProgrammingLanguage"
      (parseMToParser "ProgrammingLanguage" programmingLanguageParser)

parseIgnoredLanguageDeps :: ProgrammingLanguage -> Node Pos -> Parser IgnoreLanguageDependencies
parseIgnoredLanguageDeps pl =
  withSeq "IgnoredLanguageDeps" $
    \s -> do
      ignoreDepNames <- traverse parseYAML (V.fromList s)
      case NV.fromVector ignoreDepNames of
        Nothing -> fail "Unexpected language with no deps"
        Just nvIgnoreDepNames ->
          pure
            IgnoreLanguageDependencies
              { _programmingLanguage = pl,
                _dependencies = nvIgnoreDepNames
              }

parseAdditionalDeps :: ProgrammingLanguage -> Node Pos -> Parser (V.Vector BasicDependency)
parseAdditionalDeps pl = withSeq "PLAdditionalDependencies" (traverse (parseAdditionalDep pl) . V.fromList)

parseAdditionalDep :: ProgrammingLanguage -> Node Pos -> Parser BasicDependency
parseAdditionalDep (UnsupportedLanguage pl) (Scalar _ (SStr _)) = fail $ "Unsupported language " <> unpack pl <> " cannot have named dependency"
parseAdditionalDep pl n@(Scalar _ (SStr _)) = do
  dependencyName <- parseYAML n
  pure $ BasicDependency pl (DependencyIdentifierNamed dependencyName) Nothing
parseAdditionalDep pl (Mapping _ _ map') = do
  repoNode <- map' .: "repo"
  qualifiedRepo <- withStr "QualifiedRepo" parseQualifiedRepo repoNode
  dependencyName <- map' .:? "name"
  pure $ BasicDependency pl (DependencyIdentifierRepo qualifiedRepo dependencyName) Nothing
parseAdditionalDep _ _ = fail "Unexpected type for additional dependency"

parseQualifiedRepo :: Text -> Parser QualifiedRepo
parseQualifiedRepo = parseMToParser "QualifiedRepo" parserSimpleRepo

programmingLanguageParser :: MParser ProgrammingLanguage
programmingLanguageParser =
  M.try (M.string' "javascript" $> JavaScript)
    <|> M.try (M.string' "typescript" $> TypeScript)
    <|> M.try (M.string' "python" $> Python)
    <|> M.try (M.string' "php" $> Php)
    <|> M.try (M.string' "ruby" $> Ruby)
    <|> M.try (M.string' "haskell" $> Haskell)
    <|> M.try (M.string' "rust" $> Rust)
    <|> M.try (M.string' "csharp" $> CSharpNet)
    <|> M.try (M.string' "visualbasic" $> VisualBasicNet)
    <|> M.try (M.string' "java" $> Java)
    <|> M.try (M.string' "kotlin" $> Kotlin)
    <|> M.try (M.string' "scala" $> Scala)
    <|> M.try (M.string' "golang" $> Golang)
    <|> (M.some depNameChar <&> UnsupportedLanguage . pack)

parseMToParser :: String -> MParser a -> Text -> Parser a
parseMToParser name mParser input =
  case M.parse mParser name input of
    Right result -> pure result
    Left e -> fail ("Unable to parse " <> name <> ": " <> show @String e)
