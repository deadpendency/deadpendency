{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.RepoConfig where

import Common.Model.RepoConfig.FileLoadPlan
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import Common.Model.RepoConfig.RepoConfig
import Common.Model.RepoConfig.Rules.FewYearlyCommitsConfig
import Common.Model.RepoConfig.Rules.NoRecentCommitConfig
import Common.Model.RepoConfig.Rules.NoRecentPackageConfig
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Ecosystem
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genRepoConfig :: Gen RepoConfig
genRepoConfig = do
  additionalDependencies <- genVector (Range.constant 0 10) genBasicDependency
  ignoreDependenciesConfig <- genIgnoreDependenciesConfig
  additionalDependencyFiles <- genVector (Range.constant 0 10) genDependenciesFile
  fileLoadPlan <- genFileLoadPlan
  rulesConfig <- genRulesConfig
  pure
    RepoConfig
      { _additionalDependencies = additionalDependencies,
        _ignoreDependenciesConfig = ignoreDependenciesConfig,
        _additionalDependencyFiles = additionalDependencyFiles,
        _fileLoadPlan = fileLoadPlan,
        _rulesConfig = rulesConfig
      }

genFileLoadPlan :: Gen FileLoadPlan
genFileLoadPlan =
  Gen.choice
    [ Gen.constant FileLoadEnabled,
      Gen.constant FileLoadDisabled,
      FileLoadDisabledForLangs <$> genNonEmptyVector (Range.constant 1 1) genProgrammingLanguage
    ]

genIgnoreDependenciesConfig :: Gen IgnoreDependenciesConfig
genIgnoreDependenciesConfig =
  Gen.choice
    [ IDAll <$> genVector (Range.constant 0 10) genDependencyName,
      IDSpecific <$> genVector (Range.constant 0 10) genIgnoreLanguageDependencies
    ]

genIgnoreLanguageDependencies :: Gen IgnoreLanguageDependencies
genIgnoreLanguageDependencies = do
  programmingLanguage <- genProgrammingLanguage
  dependencyNames <- genNonEmptyVector (Range.constant 1 10) genDependencyName
  pure
    IgnoreLanguageDependencies
      { _programmingLanguage = programmingLanguage,
        _dependencies = dependencyNames
      }

genRuleStatus :: Gen RuleStatus
genRuleStatus = Gen.enumBounded

genRulesConfig :: Gen RulesConfig
genRulesConfig =
  RulesConfig
    <$> Gen.maybe genNoRecentPackageConfig
    <*> Gen.maybe genNoRecentCommitConfig
    <*> Gen.maybe genFewYearlyCommitsConfig
    <*> genRuleStatus
    <*> genRuleStatus
    <*> genRuleStatus
    <*> genRuleStatus
    <*> genRuleStatus
    <*> genRuleStatus

genNoRecentPackageConfig :: Gen NoRecentPackageConfig
genNoRecentPackageConfig = do
  warnAtMonths <- Gen.maybe genPositiveInt
  failAtMonths <- Gen.maybe genPositiveInt
  pure $
    NoRecentPackageConfig warnAtMonths failAtMonths

genNoRecentCommitConfig :: Gen NoRecentCommitConfig
genNoRecentCommitConfig = do
  warnAtMonths <- Gen.maybe genPositiveInt
  failAtMonths <- Gen.maybe genPositiveInt
  pure $
    NoRecentCommitConfig warnAtMonths failAtMonths

genFewYearlyCommitsConfig :: Gen FewYearlyCommitsConfig
genFewYearlyCommitsConfig = do
  warnAtCount <- Gen.maybe genPositiveInt
  failAtCount <- Gen.maybe genPositiveInt
  pure $
    FewYearlyCommitsConfig warnAtCount failAtCount
