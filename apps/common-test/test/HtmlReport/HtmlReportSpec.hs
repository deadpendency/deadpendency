{-# LANGUAGE DataKinds #-}

module HtmlReport.HtmlReportSpec (spec) where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.Error.ProcessingError ()
import Common.HtmlReport.Instances.OverallReport ()
import Common.Model.Config.AppVersion
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Report.DependencyErrorReason
import Common.Model.Report.DependencyErrorReport
import Common.Model.Report.DependencyErrorReports
import Common.Model.Report.DependencyLanguageReport
import Common.Model.Report.DependencyReports
import Common.Model.Report.OverallReport
import Common.Model.Report.ReportResult
import CommonTest.Gen.Model.Error
import CommonTest.Gen.Model.Report
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Pretty.Simple

spec :: Spec
spec = parallel $
  context "When generating reports" $ do
    it "should roundtrip success report with no changes" $
      hedgehog $ do
        generatedReport <- forAll genOverallReport

        let textReport = toHtmlReport generatedReport
            eitherRoundTripResultReport = fromHtmlReport textReport

            generatedWithResetTimesReport =
              generatedReport
                -- recent package single failures
                & (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @3 . traversed . #_dependencyAssessmentFailures . traversed . #_violation . _Ctor @"DAVNoRecentPackageRelease" . position @2) %~ removeTime
                -- recent package multi failures
                & (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @3 . traversed . #_reports . traversed . #_dependencyAssessmentFailures . traversed . #_violation . _Ctor @"DAVNoRecentPackageRelease" . position @2) %~ removeTime
                -- recent package single failures -> warnings
                & (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @3 . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentPackageRelease" . position @2) %~ removeTime
                -- recent package multi failures -> warnings
                & (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @3 . traversed . #_reports . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentPackageRelease" . position @2) %~ removeTime
                -- recent package single warnings
                & (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @2 . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentPackageRelease" . position @2) %~ removeTime
                -- recent package multi warnings
                & (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @2 . traversed . #_reports . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentPackageRelease" . position @2) %~ removeTime
                -- recent commit single failures
                & (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @3 . traversed . #_dependencyAssessmentFailures . traversed . #_violation . _Ctor @"DAVNoRecentCommits" . position @2 . _Just) %~ removeTime
                -- recent commit multi failures
                & (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @3 . traversed . #_reports . traversed . #_dependencyAssessmentFailures . traversed . #_violation . _Ctor @"DAVNoRecentCommits" . position @2 . _Just) %~ removeTime
                -- recent commit single failure -> warnings
                & (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @3 . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentCommits" . position @2 . _Just) %~ removeTime
                -- recent commit multi failure -> warnings
                & (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @3 . traversed . #_reports . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentCommits" . position @2 . _Just) %~ removeTime
                -- recent commit single warnings
                & (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @2 . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentCommits" . position @2 . _Just) %~ removeTime
                -- recent commit multi warnings
                & (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @2 . traversed . #_reports . traversed . #_dependencyAssessmentWarnings . traversed . #_violation . _Ctor @"DAVNoRecentCommits" . position @2 . _Just) %~ removeTime

        annotateShow generatedReport

        annotateShow textReport

        eitherRoundTripResultReport === Right generatedWithResetTimesReport

    it "should roundtrip failure report with no changes" $
      hedgehog $ do
        processingError <- forAll genProcessingError

        let textReport = toHtmlReport processingError
            eitherRoundTripReport = fromHtmlReport textReport

        annotateShow textReport

        eitherRoundTripReport === Right processingError

    xit "play area" $ do
      let startingReport =
            OverallReport
              { _producedWithVersion = AppVersion {_ntText = "1.1.1"},
                _reportResult = ReportResultPass,
                _dependencyReports =
                  DRMultiLanguageReports
                    (V.fromList [])
                    (V.fromList [])
                    (V.fromList [])
                    (V.fromList [])
                    ( V.fromList
                        [ DependencyLanguageReport
                            { _programmingLanguage = JavaScript,
                              _reports =
                                NV.unsafeFromList
                                  [ DependencyErrorReports
                                      { _erroredReason = DERNoRegistryOrRepoData,
                                        _errorReports =
                                          NV.unsafeFromList
                                            [ DependencyErrorReport
                                                { _dependencyIdentifier =
                                                    DependencyIdentifierNamed $ DependencyName {_ntText = "a"},
                                                  _dependencyPackageLink = Nothing
                                                }
                                            ]
                                      },
                                    DependencyErrorReports
                                      { _erroredReason = DERNoRegistryOrRepoData,
                                        _errorReports =
                                          NV.unsafeFromList
                                            [ DependencyErrorReport
                                                { _dependencyIdentifier =
                                                    DependencyIdentifierNamed $ DependencyName {_ntText = "a"},
                                                  _dependencyPackageLink = Nothing
                                                }
                                            ]
                                      }
                                  ]
                            }
                        ]
                    ),
                _isTruncated = False
              }

      let renderedReport = toHtmlReport startingReport

      pPrint renderedReport

      let eitherFinalReport = fromHtmlReport renderedReport

      pPrint eitherFinalReport

      Right startingReport `shouldBe` eitherFinalReport

removeTime :: UTCTime -> UTCTime
removeTime = flip UTCTime 0 . utctDay
