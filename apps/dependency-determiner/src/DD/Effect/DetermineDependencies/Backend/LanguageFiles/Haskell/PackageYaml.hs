module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.PackageYaml
  ( PackageYamlInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.HsYAML ()
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Internal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Map.Strict qualified as C
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.YAML

{-
name: check-run-creator
library:
  source-dirs: src
  dependencies:
    - servant
    - servant-server
    - github
tests:
  check-run-creator-test:
    main: Spec.hs
    source-dirs: test
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - check-run-creator
      - servant-server
      - fused-effects
      - github
-}

newtype PackageYaml = PackageYaml
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype PackageYamlInput = PackageYamlInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromYAML PackageYaml where
  parseYAML =
    withMap "PackageYaml" $ \m -> do
      name <- m .: "name"

      rootDeps <- (traverse (dependencyParser CoreDependency) =<<) $ m .:? "dependencies" .!= V.empty

      maybeLibrary <- m .:? "library"
      libraryDeps <-
        case maybeLibrary of
          Just library -> (traverse (dependencyParser CoreDependency) =<<) $ library .:? "dependencies" .!= V.empty
          Nothing -> pure V.empty
      let rootAndLibaryDeps = rootDeps <> libraryDeps

      maybeTests <- m .:? "tests"
      testDependencies <-
        case maybeTests of
          Just tests ->
            withMap
              "TestDeps"
              (fmap join . traverse testPackageToDeps . V.fromList . C.elems)
              tests
          Nothing -> pure V.empty

      maybeExecutables <- m .:? "executables"
      exeDependencies <-
        case maybeExecutables of
          Just executables ->
            withMap
              "ExeDeps"
              (fmap join . traverse testPackageToDeps . V.fromList . C.elems)
              executables
          Nothing -> pure V.empty

      maybeBenchmarks <- m .:? "benchmarks"
      benchDependencies <-
        case maybeBenchmarks of
          Just benchmarks ->
            withMap
              "BenchDeps"
              (fmap join . traverse testPackageToDeps . V.fromList . C.elems)
              benchmarks
          Nothing -> pure V.empty

      let allDeps = uniqueByName (rootAndLibaryDeps <> testDependencies <> exeDependencies <> benchDependencies)
          filteredDeps = V.filter (\b -> isNotMatchedDep name b && isNotMatchedDep "base" b) allDeps
      pure $
        PackageYaml filteredDeps

testPackageToDeps :: Node Pos -> Parser (V.Vector BasicDependency)
testPackageToDeps =
  withMap "TestDep" $ \m -> do
    maybeDeps <- m .:? "dependencies"
    case maybeDeps of
      Just deps -> traverse (dependencyParser DevDependency) deps
      Nothing -> pure V.empty

dependencyParser :: DependencyType -> Node Pos -> Parser BasicDependency
dependencyParser dependencyType =
  withStr "BasicDependency" (pure . nameToDependency dependencyType . T.takeWhile (/= ' ')) -- strip possible versions

nameToDependency :: DependencyType -> Text -> BasicDependency
nameToDependency dependencyType name = BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName name) (Just dependencyType)

instance CanDetermineDependencies PackageYamlInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile HpackPackageYaml gitPath . show @Text)
      _pjBasicDependencies
      . decode1
      . encodeUtf8
      . _pjText
