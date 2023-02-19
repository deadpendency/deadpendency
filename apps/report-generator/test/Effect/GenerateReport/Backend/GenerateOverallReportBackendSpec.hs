{-# LANGUAGE DataKinds #-}

module Effect.GenerateReport.Backend.GenerateOverallReportBackendSpec (spec) where

import Common.Model.Assessment.DependencyAssessmentResult
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Report.DependencyReports
import Common.Model.Report.ReportResult
import CommonTest.Gen.General
import CommonTest.Gen.Model.Assessment
import CommonTest.Gen.Model.Config
import CommonTest.Gen.Model.Dependency
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RG.Effect.GenerateReport.Backend.GenerateOverallReportBackend
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "generating overall report" $ do
    it "no failures results in success" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 0 10) genErroredDependency
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let massagedAssessments =
              dependencyAssessments
                & (traversed . #_dependencyAssessmentResult) .~ DARPass
        let resultStatus =
              generateOverallReport
                appVersion
                errorReports
                ignoreReports
                massagedAssessments
                ^. #_reportResult

        resultStatus === ReportResultPass

    it "any warnings results in warn" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 0 10) genErroredDependency
        warning <- Gen.sample genDependencyAssessmentResultWarning
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let massagedAssessments =
              dependencyAssessments
                & (traversed . #_dependencyAssessmentResult) .~ warning
        let resultStatus =
              generateOverallReport
                appVersion
                errorReports
                ignoreReports
                massagedAssessments
                ^. #_reportResult

        resultStatus === ReportResultWarning

    it "any failures results in failure" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 0 10) genErroredDependency
        failure' <- Gen.sample genDependencyAssessmentResultFailure
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let massagedAssessments =
              dependencyAssessments
                & (ix 0 . #_dependencyAssessmentResult) .~ failure'
        let resultStatus =
              generateOverallReport
                appVersion
                errorReports
                ignoreReports
                massagedAssessments
                ^. #_reportResult

        resultStatus === ReportResultFail

    it "any failures results in failure" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 0 10) genErroredDependency
        failure' <- Gen.sample genDependencyAssessmentResultFailure
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let massagedAssessments =
              dependencyAssessments
                & (ix 0 . #_dependencyAssessmentResult) .~ failure'
        let resultStatus =
              generateOverallReport
                appVersion
                errorReports
                ignoreReports
                massagedAssessments
                ^. #_reportResult

        resultStatus === ReportResultFail

    it "all the same languages results in a single language dependency reports" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 0 10) genErroredDependency
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let massagedErrorReports =
              errorReports
                & (traversed . #_programmingLanguage) .~ Haskell
            massagedAssessments =
              dependencyAssessments
                & (traversed . #_enrichedDependency . #_programmingLanguage) .~ Haskell

        let resultReports =
              generateOverallReport
                appVersion
                massagedErrorReports
                ignoreReports
                massagedAssessments
                ^. #_dependencyReports

        dependencyReportsIsSingle resultReports === True

    it "multi languages results in multi language dependency reports" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 1 10) genErroredDependency
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let massagedErrorReports =
              errorReports
                & (traversed . #_programmingLanguage) .~ Python
            massagedAssessments =
              dependencyAssessments
                & (traversed . #_enrichedDependency . #_programmingLanguage) .~ Haskell

        let resultReports =
              generateOverallReport
                appVersion
                massagedErrorReports
                ignoreReports
                massagedAssessments
                ^. #_dependencyReports

        annotateShow resultReports

        dependencyReportsIsSingle resultReports === False

    it "sorts the reports" $
      hedgehog $ do
        ignoreReports <- Gen.sample $ genVector (Range.constant 0 10) genIgnoredDependency
        errorReports <- Gen.sample $ genVector (Range.constant 0 10) genErroredDependency
        dependencyAssessments <- forAll $ genNonEmptyVector (Range.constant 1 10) genDependencyAssessment
        appVersion <- Gen.sample genAppVersion

        let resultReports =
              generateOverallReport
                appVersion
                errorReports
                ignoreReports
                dependencyAssessments
                ^. #_dependencyReports

            sortedResultReports =
              resultReports
                & (_Ctor @"DRSingleLanguageReports" . position @1) %~ sortV
                & (_Ctor @"DRSingleLanguageReports" . position @2) %~ sortV
                & (_Ctor @"DRSingleLanguageReports" . position @3) %~ sortV
                & (_Ctor @"DRSingleLanguageReports" . position @4) %~ sortV
                & (_Ctor @"DRSingleLanguageReports" . position @5) %~ sortV
                & (_Ctor @"DRMultiLanguageReports" . position @1 . traversed . #_reports) %~ sortNV
                & (_Ctor @"DRMultiLanguageReports" . position @2 . traversed . #_reports) %~ sortNV
                & (_Ctor @"DRMultiLanguageReports" . position @3 . traversed . #_reports) %~ sortNV
                & (_Ctor @"DRMultiLanguageReports" . position @4 . traversed . #_reports) %~ sortNV
                & (_Ctor @"DRMultiLanguageReports" . position @5 . traversed . #_reports) %~ sortNV

        annotateShow resultReports

        resultReports === sortedResultReports

dependencyReportsIsSingle :: DependencyReports -> Bool
dependencyReportsIsSingle DRSingleLanguageReports {} = True
dependencyReportsIsSingle DRMultiLanguageReports {} = False
