module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Php.ComposerJson
  ( ComposerJsonInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.NameParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Text qualified as Text
import Data.Vector qualified as V
import Text.Megaparsec qualified as M

newtype ComposerJson = ComposerJson
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype ComposerJsonInput = ComposerJsonInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ComposerJson where
  parseJSON =
    withObject "ComposerJson" $ \v -> do
      maybeDependenciesObject <- v .:? "require"
      maybeDevDependenciesObject <- v .:? "require-dev"
      dependencies <- identifierToDependency CoreDependency <<$>> depsToIdentifiers maybeDependenciesObject
      devDependencies <- identifierToDependency DevDependency <<$>> depsToIdentifiers maybeDevDependenciesObject
      pure $ ComposerJson $ dependencies <> devDependencies

identifierToDependency :: DependencyType -> DependencyIdentifier -> BasicDependency
identifierToDependency dt identifier = BasicDependency Php identifier (Just dt)

instance CanDetermineDependencies ComposerJsonInput where
  determineDependencies gitPath = bimap (UnableToParseDependencyFile PackagistComposerJson gitPath . pack) _pjBasicDependencies . eitherDecodeStrict' . encodeUtf8 . _pjText

depsToIdentifiers :: Maybe Value -> Parser (V.Vector DependencyIdentifier)
depsToIdentifiers Nothing = pure V.empty
depsToIdentifiers (Just (Object v)) =
  let eitherDepIdentifiers = (traverse (M.parse parserDependencyNamed "ComposerJson Dep Name") . V.filter validPhpDep . fmap KM.toText . V.fromList . KM.keys) v
   in case eitherDepIdentifiers of
        Left e -> fail $ "Unable to parse dependency name: " <> M.errorBundlePretty e
        Right depIds -> pure depIds
depsToIdentifiers (Just invalid) =
  prependFailure
    "parsing object to keys failed, "
    (typeMismatch "Object" invalid)

validPhpDep :: Text -> Bool
validPhpDep depName
  | depName == "php" = False
  -- deps with no slash should actually be PHP extensions ie. ext-json. They either don't exist in the registry, or will not load with just this name anyway
  | Text.all (/= '/') depName = False
  | otherwise = True
