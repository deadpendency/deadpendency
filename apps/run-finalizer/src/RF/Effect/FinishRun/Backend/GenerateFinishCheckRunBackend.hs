{-# LANGUAGE DataKinds #-}

module RF.Effect.FinishRun.Backend.GenerateFinishCheckRunBackend
  ( generateFinishCheckRun,
  )
where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportTextual
import Common.HtmlReport.Instances.Error.ProcessingError ()
import Common.HtmlReport.Instances.OverallReport ()
import Common.Model.GitHub.Checks.CheckRunConclusion
import Common.Model.GitHub.Checks.Output.CheckRunOutputBody
import Common.Model.GitHub.Checks.Output.CheckRunOutputSummary
import Common.Model.GitHub.Checks.Output.CheckRunOutputTitle
import Common.Model.InterchangeEvent.RunResult
import Common.Model.Report.OverallReport
import Common.Model.Report.ReportResult
import Data.Text qualified as Text
import RF.Effect.FinishRun.Backend.Model.FinishCheckRun

generateFinishCheckRun :: RunResult -> FinishCheckRun
generateFinishCheckRun runResult =
  let reportTextual =
        case runResult of
          RunSuccess overallReport -> generateReport overallReport
          RunFailure processingError -> toHtmlReport processingError
      title = CheckRunOutputTitle $ reportTextual ^. #_title
      summary = CheckRunOutputSummary $ reportTextual ^. #_summary
      bodyText = reportTextual ^. #_body
      body = CheckRunOutputBody bodyText

      conclusion = generateConclusion runResult
   in FinishCheckRun
        { _fcrSummary = summary,
          _fcrBody = body,
          _fcrTitle = title,
          _fcrCheckRunConclusion = conclusion
        }

generateConclusion :: RunResult -> CheckRunConclusion
generateConclusion runResult =
  case runResult of
    RunSuccess overallReport ->
      case overallReport ^. #_reportResult of
        ReportResultPass -> CheckRunConclusionSuccess
        ReportResultWarning -> CheckRunConclusionSuccess
        ReportResultFail -> CheckRunConclusionFailure
    RunFailure _ ->
      CheckRunConclusionFailure

generateReport :: OverallReport -> HtmlReportTextual
generateReport overallReport =
  let textualReport = toHtmlReport overallReport
      bodyText = textualReport ^. #_body
   in if Text.length bodyText > 65535
        then toHtmlReport $ getTruncatedReport overallReport
        else textualReport

getTruncatedReport :: OverallReport -> OverallReport
getTruncatedReport overallReport =
  overallReport
    & #_isTruncated .~ True
    & #_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @1 . traversed . #_dependencyRepo .~ Nothing
    & #_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @1 . traversed . #_dependencyPackageLink .~ Nothing
    & #_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @1 . traversed . #_reports . traversed . #_dependencyRepo .~ Nothing
    & #_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @1 . traversed . #_reports . traversed . #_dependencyPackageLink .~ Nothing
