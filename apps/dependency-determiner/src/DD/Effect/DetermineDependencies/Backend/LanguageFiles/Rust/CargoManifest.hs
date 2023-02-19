module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Rust.CargoManifest
  ( CargoManifestInput (..),
    ParsedCargoManifest (..),
    parsedCargoManifestCodec,
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

newtype CargoManifestInput = CargoManifestInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

{-
[dependencies]
time = "0.1.12"
some-crate = { version = "1.0", registry = "my-registry" }
rand = { git = "https://github.com/rust-lang-nursery/rand" }
other = { git = "https://github.com/rust-lang-nursery/rand", branch = "next" }
hello_utils = { path = "hello_utils" }

[dev-dependencies]
tempdir = "0.3"

[build-dependencies]
cc = "1.0.3"
-}

data ParsedCargoManifest = ParsedCargoManifest
  { _coreDeps :: V.Vector BasicDependency,
    _buildDeps :: V.Vector BasicDependency,
    _devDeps :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show)

getDeps :: ParsedCargoManifest -> V.Vector BasicDependency
getDeps (ParsedCargoManifest core target dev) = V.concat [core, target, dev]

parsedCargoManifestCodec :: TomlCodec ParsedCargoManifest
parsedCargoManifestCodec =
  ParsedCargoManifest
    <$> simpleDeps CoreDependency "dependencies"
      .= _coreDeps
    <*> simpleDeps DevDependency "build-dependencies"
      .= _buildDeps
    <*> simpleDeps DevDependency "dev-dependencies"
      .= _devDeps

data PackageDetails = PackageDetails
  { _pdRepo :: Maybe QualifiedRepo,
    _pdDependencyType :: DependencyType
  }
  deriving stock (Eq, Show)

simpleDeps :: DependencyType -> Toml.Key -> TomlCodec (V.Vector BasicDependency)
simpleDeps depType keyName =
  Toml.dimatch
    toMapFromVecBD
    fromMapToVecBD
    (Toml.tableHashMap Toml._KeyText (dependencyDetailsCodec depType) keyName)

toMapFromVecBD :: V.Vector BasicDependency -> Maybe (HM.HashMap Text PackageDetails)
toMapFromVecBD basicDeps =
  HM.fromList . V.toList <$> traverse basicDepToTupleWithName basicDeps

fromMapToVecBD :: HM.HashMap Text PackageDetails -> V.Vector BasicDependency
fromMapToVecBD = fmap fromKeyValue . V.fromList . HM.toList

fromKeyValue :: (Text, PackageDetails) -> BasicDependency
fromKeyValue (name, PackageDetails maybeRepo depType) =
  case maybeRepo of
    Just repo -> BasicDependency Rust (DependencyIdentifierRepo repo (Just $ DependencyName $ trimQuotes name)) (Just depType)
    Nothing -> BasicDependency Rust (DependencyIdentifierNamed $ DependencyName $ trimQuotes name) (Just depType)

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
  first (show @Text) . M.parse parserQualifiedRepo "QualifiedRepo"

fromQualifiedRepo :: QualifiedRepo -> Text
fromQualifiedRepo (QualifiedRepo _ (RepoOwner owner) (RepoName name)) =
  "https://github.com/" <> owner <> "/" <> name <> ".git"

basicDepToTupleWithName :: BasicDependency -> Maybe (Text, PackageDetails)
basicDepToTupleWithName (BasicDependency _ dependencyIdentifier maybeDependencyType) =
  dependencyIdentifierToName dependencyIdentifier
    >>= (\name -> maybeDependencyType <&> (name,))
    <&> fmap (PackageDetails (dependencyIdentifierToQualifiedRepo dependencyIdentifier))

instance CanDetermineDependencies CargoManifestInput where
  determineDependencies gitPath =
    bimap (UnableToParseDependencyFile CratesCargoToml gitPath . show @Text) getDeps . Toml.decode parsedCargoManifestCodec . _pjText
