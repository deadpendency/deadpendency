module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.PyProjectToml
  ( PyProjectTomlInput (..),
    ParsedPyProjectToml (..),
    parsedPyProjectTomlCodec,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Parsing.NameParsing
import Common.Parsing.RepoParsing
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Internal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Toml (TomlCodec, (.=))
import Toml qualified

newtype PyProjectTomlInput = PyProjectTomlInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

data ParsedPyProjectToml = ParsedPyProjectToml
  { _coreDeps :: V.Vector BasicDependency,
    _devDeps :: V.Vector BasicDependency,
    _coreProjectDeps :: V.Vector BasicDependency,
    _coreProjectOptionalDeps :: V.Vector BasicDependency,
    _buildDeps :: V.Vector BasicDependency,
    _poetryDeps :: V.Vector BasicDependency,
    _poetryDevDeps :: V.Vector BasicDependency,
    _flitDeps :: V.Vector BasicDependency,
    _flitExtraTest :: V.Vector BasicDependency,
    _flitExtraDoc :: V.Vector BasicDependency,
    _flitExtraDev :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show)

getDeps :: ParsedPyProjectToml -> V.Vector BasicDependency
getDeps (ParsedPyProjectToml core dev coreProject coreProjectOptional build poetryCore poetryDev flitDeps flitExtrasTest flitExtrasDoc flitExtrasDev) =
  V.concat [core, dev, coreProject, coreProjectOptional, build, poetryCore, poetryDev, flitDeps, flitExtrasTest, flitExtrasDoc, flitExtrasDev]

parsedPyProjectTomlCodec :: TomlCodec ParsedPyProjectToml
parsedPyProjectTomlCodec =
  ParsedPyProjectToml
    <$> deps CoreDependency "dependencies"
      .= _coreDeps
    <*> deps DevDependency "dev-dependencies"
      .= _devDeps
    <*> namedSection CoreDependency "project" "dependencies"
      .= _coreProjectDeps
    <*> namedSectionChildren DevDependency "project.optional-dependencies"
      .= _coreProjectOptionalDeps
    <*> namedSection DevDependency "build-system" "requires"
      .= _buildDeps
    <*> deps CoreDependency "tool.poetry.dependencies"
      .= _poetryDeps
    <*> deps DevDependency "tool.poetry.dev-dependencies"
      .= _poetryDevDeps
    <*> namedSection CoreDependency "tool.flit.metadata" "requires"
      .= _flitDeps
    <*> namedSection DevDependency "tool.flit.metadata.requires-extra" "test"
      .= _flitExtraTest
    <*> namedSection DevDependency "tool.flit.metadata.requires-extra" "doc"
      .= _flitExtraDoc
    <*> namedSection DevDependency "tool.flit.metadata.requires-extra" "dev"
      .= _flitExtraDev

data PackageDetails = PackageDetails
  { _pdRepo :: Maybe QualifiedRepo,
    _pdDependencyType :: DependencyType
  }
  deriving stock (Eq, Show)

namedSectionChildren :: DependencyType -> Toml.Key -> TomlCodec (V.Vector BasicDependency)
namedSectionChildren depType rootKey =
  Toml.dimatch
    (const Nothing)
    (fmap (nameToStrippedBd depType) . V.concat . fmap V.fromList . HM.elems)
    (Toml.tableHashMap Toml._KeyText (Toml.arrayOf Toml._String) rootKey)

namedSection :: DependencyType -> Toml.Key -> Toml.Key -> TomlCodec (V.Vector BasicDependency)
namedSection depType rootKey depListName =
  Toml.dimap
    Just
    (fromMaybe V.empty)
    (Toml.dioptional (Toml.table (depsFromArray depType depListName) rootKey))

depsFromArray :: DependencyType -> Toml.Key -> TomlCodec (V.Vector BasicDependency)
depsFromArray depType depListName =
  Toml.dimatch
    (fmap V.toList . traverse bdToName)
    (fmap (nameToStrippedBd depType) . V.fromList)
    (Toml.arrayOf Toml._String depListName)

nameToStrippedBd :: DependencyType -> String -> BasicDependency
nameToStrippedBd depType name =
  BasicDependency Python (DependencyIdentifierNamed $ DependencyName $ takeDepNameChars $ trimQuotes $ pack name) (Just depType)

bdToName :: BasicDependency -> Maybe String
bdToName bd =
  bd ^? (#_dependencyIdentifier . to getDIName . _Just . #_ntText . to unpack)

deps :: DependencyType -> Toml.Key -> TomlCodec (V.Vector BasicDependency)
deps depType keyName =
  Toml.dimatch
    toMapFromVecBD
    fromMapToVecBD
    (Toml.tableHashMap Toml._KeyText (dependencyDetailsCodec depType) keyName)

toMapFromVecBD :: V.Vector BasicDependency -> Maybe (HM.HashMap Text PackageDetails)
toMapFromVecBD basicDeps =
  HM.fromList . V.toList <$> traverse basicDepToTupleWithName basicDeps

fromMapToVecBD :: HM.HashMap Text PackageDetails -> V.Vector BasicDependency
fromMapToVecBD = fmap fromKeyValue . V.fromList . HM.toList
  where
    fromKeyValue :: (Text, PackageDetails) -> BasicDependency
    fromKeyValue (name, PackageDetails maybeRepo depType) =
      case maybeRepo of
        Just repo -> BasicDependency Python (DependencyIdentifierRepo repo (Just $ DependencyName $ trimQuotes name)) (Just depType)
        Nothing -> BasicDependency Python (DependencyIdentifierNamed $ DependencyName $ trimQuotes name) (Just depType)

trimQuotes :: Text -> Text
trimQuotes = Text.takeWhile (\c -> c /= '\"' && c /= '\'') . Text.dropWhile (\c -> c == '\"' || c == '\'')

dependencyDetailsCodec :: DependencyType -> Toml.Key -> TomlCodec PackageDetails
dependencyDetailsCodec dependencyType key =
  PackageDetails
    <$> Toml.dioptional (Toml.table qualifiedRepoCodec key)
      .= _pdRepo
    <*> pure dependencyType
      .= _pdDependencyType

qualifiedRepoCodec :: TomlCodec QualifiedRepo
qualifiedRepoCodec =
  Toml.textBy fromQualifiedRepo toQualifiedRepo "git"

toQualifiedRepo :: Text -> Either Text QualifiedRepo
toQualifiedRepo =
  first show . M.parse parserQualifiedRepo "QualifiedRepo"

fromQualifiedRepo :: QualifiedRepo -> Text
fromQualifiedRepo (QualifiedRepo _ (RepoOwner owner) (RepoName name)) =
  "https://github.com/" <> owner <> "/" <> name <> ".git"

basicDepToTupleWithName :: BasicDependency -> Maybe (Text, PackageDetails)
basicDepToTupleWithName (BasicDependency _ dependencyIdentifier maybeDependencyType) =
  dependencyIdentifierToName dependencyIdentifier
    >>= (\name -> maybeDependencyType <&> (name,))
    <&> fmap (PackageDetails (dependencyIdentifierToQualifiedRepo dependencyIdentifier))

instance CanDetermineDependencies PyProjectTomlInput where
  determineDependencies gitPath =
    bimap (DependencyFileInvalid PyProjectToml gitPath . show @Text) getDeps . Toml.decode parsedPyProjectTomlCodec . _pjText
