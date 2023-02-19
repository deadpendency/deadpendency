module DD.Effect.DetermineDependencies.Backend.LanguageFiles.JavaScript.PackageJson
  ( PackageJsonInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.QualifiedRepo
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

newtype PackageJson = PackageJson
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype PackageJsonInput = PackageJsonInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON PackageJson where
  parseJSON =
    withObject "PackageJson" $ \v -> do
      maybeDependenciesObject <- v .:? "dependencies"
      maybeDevDependenciesObject <- v .:? "devDependencies"
      maybeOptionalDependenciesObject <- v .:? "optionalDependencies"
      dependencies <- identifierToDependency CoreDependency <<$>> depsToIdentifiers maybeDependenciesObject
      devDependencies <- identifierToDependency DevDependency <<$>> depsToIdentifiers maybeDevDependenciesObject
      optionalDependencies <- identifierToDependency CoreDependency <<$>> depsToIdentifiers maybeOptionalDependenciesObject
      pure $ PackageJson $ dependencies <> devDependencies <> optionalDependencies

identifierToDependency :: DependencyType -> DependencyIdentifier -> BasicDependency
identifierToDependency dt identifier = BasicDependency JavaScript identifier (Just dt)

parserNpmRepo :: MParser QualifiedRepo
parserNpmRepo = M.try parserNpmQualifiedRepo <|> parserNpmSimpleRepo

parserNpmQualifiedRepo :: MParser QualifiedRepo
parserNpmQualifiedRepo = parserQualifiedRepo <* M.optional (M.char '#' *> M.someTill M.anySingle M.eof)

parserNpmSimpleRepo :: MParser QualifiedRepo
parserNpmSimpleRepo = parserSimpleRepo <* M.optional (M.char '#' *> M.someTill M.anySingle M.eof)

instance CanDetermineDependencies PackageJsonInput where
  determineDependencies gitPath =
    -- haven't had any package.json problems parsing in ages and many people have invalid ones
    bimap (DependencyFileInvalid NpmPackageJson gitPath . pack) _pjBasicDependencies . eitherDecodeStrict' . encodeUtf8 . _pjText

depsToIdentifiers :: Maybe Value -> Parser (V.Vector DependencyIdentifier)
depsToIdentifiers Nothing = pure V.empty
depsToIdentifiers (Just (Object v)) = V.fromList . KM.elems <$> KM.traverseWithKey depToIdentifier v
depsToIdentifiers (Just invalid) =
  prependFailure
    "parsing object to keys failed, "
    (typeMismatch "Object" invalid)

depToIdentifier :: KM.Key -> Value -> Parser DependencyIdentifier
depToIdentifier depNameKey (String depText) =
  let depName = KM.toText depNameKey
   in case depTextToRepo depText of
        Just repo -> pure $ DependencyIdentifierRepo repo (Just $ DependencyName depName)
        Nothing -> pure $ DependencyIdentifierNamed $ DependencyName depName
depToIdentifier depNameKey (Object _) = pure $ DependencyIdentifierNamed $ DependencyName (KM.toText depNameKey)
depToIdentifier depNameKey invalid =
  prependFailure
    ("parsing dep identifier unexpected value for name: " <> unpack (KM.toText depNameKey))
    (typeMismatch "String" invalid)

depTextToRepo :: Text -> Maybe QualifiedRepo
depTextToRepo = mParseMaybe parserNpmRepo
