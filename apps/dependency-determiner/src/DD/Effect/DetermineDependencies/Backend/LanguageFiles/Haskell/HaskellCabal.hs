module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.HaskellCabal
  ( HaskellCabalInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Internal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V
import Distribution.PackageDescription.Parsec qualified as C
import Distribution.Types.CondTree qualified as C
import Distribution.Types.Dependency qualified as C
import Distribution.Types.GenericPackageDescription qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import Distribution.Types.PackageName qualified as C

newtype HaskellCabalFile = HaskellCabalFile
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype HaskellCabalInput = HaskellCabalInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

parseCabalText :: Text -> Either Text (V.Vector BasicDependency)
parseCabalText input = do
  let asBS = encodeUtf8 input
  gpd <- toNiceError $ C.parseGenericPackageDescription asBS
  let packageName = pack . C.unPackageName . C.pkgName . C.package . C.packageDescription $ gpd
      coreDeps =
        case C.condLibrary gpd of
          Just condLibrary -> getDepsFromCondTree CoreDependency condLibrary
          Nothing -> V.empty
      subLibraryDeps = V.fromList (C.condSubLibraries gpd) >>= getDepsFromCondTree CoreDependency . snd
      foreignLibraryDeps = V.fromList (C.condForeignLibs gpd) >>= getDepsFromCondTree CoreDependency . snd
      testDeps = V.fromList (C.condTestSuites gpd) >>= getDepsFromCondTree DevDependency . snd
      benchDeps = V.fromList (C.condBenchmarks gpd) >>= getDepsFromCondTree DevDependency . snd
      exeDeps = V.fromList (C.condExecutables gpd) >>= getDepsFromCondTree DevDependency . snd
      allDeps = uniqueByName (coreDeps <> subLibraryDeps <> foreignLibraryDeps <> testDeps <> benchDeps <> exeDeps)
      filteredBase = V.filter (\b -> isNotMatchedDep packageName b && isNotMatchedDep "base" b) allDeps
  pure filteredBase

getDepsFromCondTree :: forall v a. DependencyType -> C.CondTree v [C.Dependency] a -> V.Vector BasicDependency
getDepsFromCondTree depType tree =
  fmap (nameToDependency depType . C.unPackageName . C.depPkgName) . V.fromList $
    C.condTreeConstraints tree

toNiceError :: C.ParseResult C.GenericPackageDescription -> Either Text C.GenericPackageDescription
toNiceError = first (("Unable to parse cabal file: " <>) . show) . snd . C.runParseResult

nameToDependency :: DependencyType -> String -> BasicDependency
nameToDependency depType name = BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName $ pack name) (Just depType)

instance CanDetermineDependencies HaskellCabalInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile HaskellCabal gitPath)
      _pjBasicDependencies
      . fmap HaskellCabalFile
      . parseCabalText
      . _pjText
