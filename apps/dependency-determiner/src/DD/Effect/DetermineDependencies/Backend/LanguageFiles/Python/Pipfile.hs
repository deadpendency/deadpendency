module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.Pipfile
  ( PipfileInput (..),
    ParsedPipfile (..),
    parsedPipfileCodec,
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

newtype PipfileInput = PipfileInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

{-
https://github.com/pypa/pipfile#examples-spec-v6
[packages]
requests = { extras = ['socks'] }
records = '>0.5.0'
django = { git = 'https://github.com/django/django.git', ref = '1.11.4', editable = true }
"e682b37" = {file = "https://github.com/divio/django-cms/archive/release/3.4.x.zip"}
"e1839a8" = {path = ".", editable = true}
pywinusb = { version = "*", os_name = "=='nt'", index="pypi"}
[dev-packages]
nose = '*'
unittest2 = {version = ">=1.0,<3.0", markers="python_version < '2.7.9' or (python_version >= '3.0' and python_version < '3.
-}

data ParsedPipfile = ParsedPipfile
  { _coreDeps :: V.Vector BasicDependency,
    _devDeps :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show)

getDeps :: ParsedPipfile -> V.Vector BasicDependency
getDeps (ParsedPipfile core dev) = core V.++ dev

parsedPipfileCodec :: TomlCodec ParsedPipfile
parsedPipfileCodec =
  ParsedPipfile
    <$> deps CoreDependency "packages"
      .= _coreDeps
    <*> deps DevDependency "dev-packages"
      .= _devDeps

data PackageDetails = PackageDetails
  { _pdRepo :: Maybe QualifiedRepo,
    _pdDependencyType :: DependencyType
  }
  deriving stock (Eq, Show)

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

instance CanDetermineDependencies PipfileInput where
  determineDependencies gitPath = bimap (UnableToParseDependencyFile PipenvPipfile gitPath . show @Text) getDeps . Toml.decode parsedPipfileCodec . _pjText
