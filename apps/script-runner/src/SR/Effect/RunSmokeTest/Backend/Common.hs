{-# LANGUAGE DataKinds #-}

module SR.Effect.RunSmokeTest.Backend.Common
  ( checkRunOutputToHtmlReportTextual,
    pollForNewCheckRunCompleted,
    getDepList,
    getErroredDepList,
  )
where

import Common.GitHub.GetCheckRunOutput.GetCheckRunOutput
import Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputRequest
import Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputResult
import Common.HtmlReport.HtmlReportTextual
import Common.HtmlReport.Instances.OverallReport ()
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.Checks.CheckRunStatus
import Common.Model.GitHub.Checks.Output.CheckRunOutput
import Common.Model.GitHub.GHNodeId
import Common.Model.Report.OverallReport
import Control.Concurrent (threadDelay)
import System.IO.Error (ioError, userError)

checkRunOutputToHtmlReportTextual :: CheckRunOutput -> HtmlReportTextual
checkRunOutputToHtmlReportTextual checkRunOutput = HtmlReportTextual title summary body
  where
    title = checkRunOutput ^. (#_checkRunOutputTitle . #_ntText)
    summary = checkRunOutput ^. (#_checkRunOutputSummary . #_ntText)
    body = checkRunOutput ^. (#_checkRunOutputBody . #_ntText)

-- waits 5 second before polling again. polling too much can result in github abuse detection being triggered
pollForNewCheckRunCompleted :: GHNodeId -> GHInstallationAuth -> GetCheckRunOutputRequest -> IO GetCheckRunOutputResult
pollForNewCheckRunCompleted oldCheckRunId auth request = do
  eitherResult <- githubGetCheckRunOutput auth request
  result <- case eitherResult of
    Right result' -> pure result'
    Left e -> ioError (userError $ "Unable to fetch check run: " <> show @String e)
  let status = result ^. #_status
      checkRunId = result ^. #_checkRunId
  case status of
    CheckRunStatusCompleted ->
      if checkRunId /= oldCheckRunId
        then pure result
        else -- seems sometimes this will run to quickly and get the old check run report (and fail due to unchanged check run id)
          threadDelay 5000000 *> pollForNewCheckRunCompleted oldCheckRunId auth request
    _ -> threadDelay 5000000 *> pollForNewCheckRunCompleted oldCheckRunId auth request

getDepList :: OverallReport -> [DependencyIdentifier]
getDepList report =
  sort $ -- need to sort as deps moving between lists changes the order
    report
      ^.. folding
        ( \s ->
            s ^.. (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @1 . folded . #_dependencyIdentifier)
              <> s ^.. (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @1 . folded . #_reports . folded . #_dependencyIdentifier)
              <> s ^.. (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @2 . folded . #_dependencyIdentifier)
              <> s ^.. (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @2 . folded . #_reports . folded . #_dependencyIdentifier)
              <> s ^.. (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @3 . folded . #_dependencyIdentifier)
              <> s ^.. (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @3 . folded . #_reports . folded . #_dependencyIdentifier)
        )

getErroredDepList :: OverallReport -> [DependencyIdentifier]
getErroredDepList report =
  report
    ^.. folding
      ( \s ->
          s ^.. (#_dependencyReports . _Ctor @"DRSingleLanguageReports" . position @5 . folded . #_errorReports . folded . #_dependencyIdentifier)
            <> s ^.. (#_dependencyReports . _Ctor @"DRMultiLanguageReports" . position @5 . folded . #_reports . folded . #_errorReports . folded . #_dependencyIdentifier)
      )
