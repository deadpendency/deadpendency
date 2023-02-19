{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Dependency where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.Basic.BasicRepoDependencies
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Dependency.Enriched.EnrichedRepoDependencies
import Common.Model.Dependency.Errored.ErroredDependency
import Common.Model.Dependency.Errored.ErroredReason
import Common.Model.Dependency.Errored.ErroredRepoDependencies
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.Dependency.File.DependenciesFileLoadDetails
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Dependency.Ignored.IgnoredDependency
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Common.Model.Dependency.Repo.DependencyRepoStats
import CommonTest.Gen.General
import CommonTest.Gen.Model.Ecosystem
import CommonTest.Gen.Model.Git
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genBasicRepoDependencies :: Gen BasicRepoDependencies
genBasicRepoDependencies = BasicRepoDependencies <$> genNonEmptyVector (Range.constant 1 10) genBasicDependency

genBasicDependency :: Gen BasicDependency
genBasicDependency = do
  programmingLanguage <- genProgrammingLanguage
  depIdentifier <- genDependencyIdentifier
  depType <- Gen.maybe genDependencyType
  pure $
    BasicDependency programmingLanguage depIdentifier depType

genDependencyIdentifier :: Gen DependencyIdentifier
genDependencyIdentifier =
  Gen.choice
    [ genDependencyIdentifierNamed,
      genDependencyIdentifierRepo
    ]

genDependencyIdentifierNamed :: Gen DependencyIdentifier
genDependencyIdentifierNamed = DependencyIdentifierNamed <$> genDependencyName

genDependencyIdentifierRepo :: Gen DependencyIdentifier
genDependencyIdentifierRepo = DependencyIdentifierRepo <$> genQualifiedRepo <*> Gen.maybe genDependencyName

genIgnoredRepoDependencies :: Gen IgnoredRepoDependencies
genIgnoredRepoDependencies = IgnoredRepoDependencies <$> genVector (Range.constant 1 10) genIgnoredDependency

-- genDependencyIdentifierOnlyNamed :: Gen DependencyIdentifier
-- genDependencyIdentifierOnlyNamed =
--   Gen.choice
--     [ genDependencyIdentifierNamed,
--       DependencyIdentifierRepo <$> genQualifiedRepo <*> (Just <$> genDependencyName)
--     ]

genIgnoredDependency :: Gen IgnoredDependency
genIgnoredDependency = do
  programmingLanguage <- genProgrammingLanguage
  dependencyIdentifier <- genDependencyIdentifier
  depType <- Gen.maybe genDependencyType
  pure
    IgnoredDependency
      { _programmingLanguage = programmingLanguage,
        _dependencyIdentifier = dependencyIdentifier,
        _dependencyType = depType
      }

genErroredRepoDependencies :: Gen ErroredRepoDependencies
genErroredRepoDependencies = ErroredRepoDependencies <$> genVector (Range.constant 1 10) genErroredDependency

genErroredDependency :: Gen ErroredDependency
genErroredDependency = do
  dependencyIdentifier <- genDependencyIdentifier
  depType <- Gen.maybe genDependencyType
  programmingLanguage <- genProgrammingLanguage
  registryInfo <- Gen.maybe genDependencyRegistryInfo
  failureReason <- genErroredReason
  pure
    ErroredDependency
      { _dependencyIdentifier = dependencyIdentifier,
        _dependencyType = depType,
        _programmingLanguage = programmingLanguage,
        _registryInfo = registryInfo,
        _erroredReason = failureReason
      }

genErroredReason :: Gen ErroredReason
genErroredReason =
  Gen.choice
    [ UnexpectedFailureToParseRegistryEntry <$> genAlphaText,
      UnexpectedFailureRegistryDataInconsistent <$> genAlphaText,
      Gen.constant NoRegistryOrRepoData
    ]

genDependencyType :: Gen DependencyType
genDependencyType = Gen.element [CoreDependency, DevDependency]

genDependencyName :: Gen DependencyName
genDependencyName =
  DependencyName
    <$> Gen.text
      (Range.constant 1 10)
      ( Gen.choice
          [ Gen.alphaNum,
            Gen.constant '-',
            Gen.constant '_',
            Gen.constant '@',
            Gen.constant '.',
            Gen.constant '/'
          ]
      )

genEnrichedRepoDependencies :: Gen EnrichedRepoDependencies
genEnrichedRepoDependencies = EnrichedRepoDependencies <$> genNonEmptyVector (Range.constant 1 10) genEnrichedDependency

genEnrichedDependency :: Gen EnrichedDependency
genEnrichedDependency = do
  programmingLanguage <- genProgrammingLanguage
  dependencyIdentifier <- genDependencyIdentifier
  dependencyType <- Gen.maybe genDependencyType
  theseData <- genThese genDependencyRegistryInfo genDependencyRepoStats
  pure
    EnrichedDependency
      { _programmingLanguage = programmingLanguage,
        _dependencyIdentifier = dependencyIdentifier,
        _dependencyType = dependencyType,
        _data = theseData
      }

genDependencyRegistryInfo :: Gen DependencyRegistryInfo
genDependencyRegistryInfo =
  DependencyRegistryInfo
    <$> genRegistry
    <*> Gen.maybe genRepo
    <*> genRegistryAlivenessStatus
    <*> Gen.maybe genUTCTime

genRegistryAlivenessStatus :: Gen RegistryAlivenessStatus
genRegistryAlivenessStatus =
  Gen.choice
    [ Gen.constant RASAlive,
      RASDeprecated <$> genRegistryAlivenessStatusType <*> Gen.maybe genAlphaText <*> genVector (Range.constant 0 10) genDependencyName
    ]

genRegistryAlivenessStatusType :: Gen RegistryAlivenessStatusType
genRegistryAlivenessStatusType = Gen.enumBounded

genDependencyRepoStats :: Gen DependencyRepoStats
genDependencyRepoStats = do
  commits <- genVector (Range.constant 0 10) genDependencyRepoCommit
  isArchived <- Gen.bool
  isFork <- Gen.bool
  pure
    DependencyRepoStats
      { _twoYearlyCommitHistory = commits,
        _isArchived = isArchived,
        _isFork = isFork
      }

genDependencyRepoCommit :: Gen DependencyRepoCommit
genDependencyRepoCommit =
  DependencyRepoCommit
    <$> genUTCTime
    <*> Gen.maybe genAlphaText

genDependenciesFileType :: Gen DependenciesFileType
genDependenciesFileType = Gen.enumBounded

genDependenciesFile :: Gen DependenciesFileLoad
genDependenciesFile =
  DependenciesFileLoad
    <$> genDependenciesFileType
    <*> genDependenciesFileLoadDetails

genDependenciesFileLoadDetails :: Gen DependenciesFileLoadDetails
genDependenciesFileLoadDetails =
  Gen.choice
    [ DFLDSpecific <$> genAlphaText,
      DFLDSearch <$> genGitFileMatch,
      DFLDDirectorySearch <$> genGitPath <*> genGitFileMatch
    ]
