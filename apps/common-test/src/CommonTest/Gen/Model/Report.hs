{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Report where

import Common.Model.Report.DependencyErrorReason
import Common.Model.Report.DependencyErrorReport
import Common.Model.Report.DependencyErrorReports
import Common.Model.Report.DependencyFailureReport
import Common.Model.Report.DependencyIgnoreReport
import Common.Model.Report.DependencyLanguageReport
import Common.Model.Report.DependencyPassReport
import Common.Model.Report.DependencyReports
import Common.Model.Report.DependencyWarningReport
import Common.Model.Report.OverallReport
import Common.Model.Report.PackageLink
import Common.Model.Report.ReportResult
import CommonTest.Gen.General
import CommonTest.Gen.Model.Assessment
import CommonTest.Gen.Model.Config
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Ecosystem
import CommonTest.Gen.Model.Git
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genDependencyPassReport :: Gen DependencyPassReport
genDependencyPassReport =
  DependencyPassReport
    <$> genDependencyIdentifier
    <*> Gen.maybe genRepo
    <*> Gen.maybe genPackageLink

genDependencyWarningReport :: Gen DependencyWarningReport
genDependencyWarningReport =
  DependencyWarningReport
    <$> genDependencyIdentifier
    <*> Gen.maybe genRepo
    <*> Gen.maybe genPackageLink
    <*> genNonEmptyVector (Range.constant 1 10) genDependencyAssessmentWarning

genDependencyFailureReport :: Gen DependencyFailureReport
genDependencyFailureReport =
  DependencyFailureReport
    <$> genDependencyIdentifier
    <*> Gen.maybe genRepo
    <*> Gen.maybe genPackageLink
    <*> genVector (Range.constant 0 10) genDependencyAssessmentWarning
    <*> genNonEmptyVector (Range.constant 1 10) genDependencyAssessmentFailure

genDependencyIgnoreReport :: Gen DependencyIgnoreReport
genDependencyIgnoreReport =
  DependencyIgnoreReport
    <$> genDependencyIdentifier

genPackageLink :: Gen PackageLink
genPackageLink = do
  registry <- genRegistry
  dependencyName <- genDependencyName
  pure
    PackageLink
      { _registry = registry,
        _dependencyName = dependencyName
      }

genDependencyErrorReports :: Gen DependencyErrorReports
genDependencyErrorReports = do
  dependencyErrorReason <- genDependencyErrorReason
  errorReports <- genNonEmptyVector (Range.constant 1 10) genDependencyErrorReport
  pure
    DependencyErrorReports
      { _erroredReason = dependencyErrorReason,
        _errorReports = errorReports
      }

genDependencyErrorReport :: Gen DependencyErrorReport
genDependencyErrorReport = do
  dependencyIdentifier <- genDependencyIdentifier
  packageLink <- Gen.maybe genPackageLink
  pure
    DependencyErrorReport
      { _dependencyIdentifier = dependencyIdentifier,
        _dependencyPackageLink = packageLink
      }

genDependencyErrorReason :: Gen DependencyErrorReason
genDependencyErrorReason = Gen.enumBounded

genDependencyReports :: Gen DependencyReports
genDependencyReports = do
  Gen.choice [genSingleLanguageDependencyReports, genMultiLanguageDependencyReports]

genMultiLanguageDependencyReports :: Gen DependencyReports
genMultiLanguageDependencyReports = Gen.filter notAllEmptyDR $ do
  passLanguageReports <- genVector (Range.constant 0 10) (genDependencyLanguageReport genDependencyPassReport)
  warningLanguageReports <- genVector (Range.constant 0 10) (genDependencyLanguageReport genDependencyWarningReport)
  failureLanguageReports <- genVector (Range.constant 0 10) (genDependencyLanguageReport genDependencyFailureReport)
  ignoreLanguageReports <- genVector (Range.constant 0 10) (genDependencyLanguageReport genDependencyIgnoreReport)
  errorLanguageReports <- genVector (Range.constant 0 10) (genDependencyLanguageReport genDependencyErrorReports)
  pure $
    DRMultiLanguageReports passLanguageReports warningLanguageReports failureLanguageReports ignoreLanguageReports errorLanguageReports

genSingleLanguageDependencyReports :: Gen DependencyReports
genSingleLanguageDependencyReports = Gen.filter notAllEmptyDR $ do
  passReports <- genVector (Range.constant 0 10) genDependencyPassReport
  warningReports <- genVector (Range.constant 0 10) genDependencyWarningReport
  failureReports <- genVector (Range.constant 0 10) genDependencyFailureReport
  ignoreReports <- genVector (Range.constant 0 10) genDependencyIgnoreReport
  errorReports <- genVector (Range.constant 0 10) genDependencyErrorReports
  pure $
    DRSingleLanguageReports passReports warningReports failureReports ignoreReports errorReports

-- dependency reports should never exist with no reports, that would be a processing failure report
-- it actually causes problems trying to round trip this case, so we exclude it
notAllEmptyDR :: DependencyReports -> Bool
notAllEmptyDR (DRSingleLanguageReports passReports warnReports failReports ignoreReports errorReports) =
  not (V.null passReports)
    || not (V.null warnReports)
    || not (V.null failReports)
    || not (V.null ignoreReports)
    || not (V.null errorReports)
notAllEmptyDR (DRMultiLanguageReports passReports warnReports failReports ignoreReports errorReports) =
  not (V.null passReports)
    || not (V.null warnReports)
    || not (V.null failReports)
    || not (V.null ignoreReports)
    || not (V.null errorReports)

genDependencyLanguageReport :: Gen a -> Gen (DependencyLanguageReport a)
genDependencyLanguageReport genReport = do
  programmingLanguage <- genProgrammingLanguage
  reports <- genNonEmptyVector (Range.constant 1 10) genReport

  pure $
    DependencyLanguageReport
      { _programmingLanguage = programmingLanguage,
        _reports = reports
      }

genOverallReport :: Gen OverallReport
genOverallReport = do
  appVersion <- genAppVersion
  reportResult <- genReportResult
  dependencyReports <- genDependencyReports
  isTruncated <- Gen.bool
  pure
    OverallReport
      { _producedWithVersion = appVersion,
        _reportResult = reportResult,
        _dependencyReports = dependencyReports,
        _isTruncated = isTruncated
      }

genReportResult :: Gen ReportResult
genReportResult = Gen.enumBounded
