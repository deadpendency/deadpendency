{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.OverallReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.HtmlReportTextual
import Common.HtmlReport.Instances.AppVersion ()
import Common.HtmlReport.Instances.DependencyFailureReport ()
import Common.HtmlReport.Instances.DependencyIgnoreReport ()
import Common.HtmlReport.Instances.DependencyPassReport ()
import Common.HtmlReport.Instances.DependencyReports ()
import Common.HtmlReport.Instances.DependencyWarningReport ()
import Common.HtmlReport.Instances.Error.DependencyErrorReport ()
import Common.Model.Report.OverallReport
import Common.Model.Report.ReportResult
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lucid
import Text.HTML.TagSoup qualified as T
import Text.HTML.TagSoup.Match qualified as T

instance ToHtmlReport OverallReport where
  toHtmlReport o@(OverallReport _ result _ _) =
    let body = fromLazy . renderText $ toHtmlReportBody o
        summary =
          case result of
            ReportResultPass -> "All dependencies are healthy! \x1F60A"
            ReportResultWarning -> "All dependencies are passing, however there are some warnings. \x1F912"
            ReportResultFail -> "Unhealthy dependencies detected! \129503"
        title = "Deadpendency Check Result"
     in HtmlReportTextual
          { _title = title,
            _summary = summary,
            _body = body
          }

instance ToHtmlReportBody OverallReport where
  toHtmlReportBody (OverallReport version _ dependencyReports isTruncated) = do
    h2_ "Dependency Reports"
    if isTruncated
      then p_ $ do
        "Due to GitHub limitations, the complete report has not been shown. "
        a_ [href_ "https://deadpendency.com/docs/troubleshooting#report-truncation"] "Learn more"
        "."
      else mempty
    toHtmlReportBody dependencyReports
    hr_ []
    if not isTruncated
      then
        p_ $
          a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "Report an issue / feature request"
            <> " or "
            <> a_ [href_ "https://deadpendency.com/contact"] "give general feedback"
            <> "."
      else mempty
    toHtmlReportBody version

instance FromHtmlReport OverallReport where
  fromHtmlReport (HtmlReportTextual _ summary body) = do
    let htmlDecodedBody = fromLazy $ toLazyText $ htmlEncodedText body
        bodyTags = T.parseTags htmlDecodedBody
        excludeFooterTags = takeWhile (T.~/= ("<hr>" :: String)) bodyTags
        versionTags = dropWhile (T.~/= ("<p id=deadpendency-version>" :: String)) bodyTags
        isTruncated = any (T.tagOpen (== "a") (elem ("href", "https://deadpendency.com/docs/troubleshooting#report-truncation"))) excludeFooterTags

    dependencyReports <- fromHtmlReportBody $ T.renderTags excludeFooterTags
    version <- fromHtmlReportBody $ T.renderTags versionTags

    reportResult <-
      case summary of
        "All dependencies are healthy! \x1F60A" -> Right ReportResultPass
        "All dependencies are passing, however there are some warnings. \x1F912" -> Right ReportResultWarning
        "Unhealthy dependencies detected! \x1F9DF" -> Right ReportResultFail
        unmatched -> Left . HtmlReportDecodeError $ "Unable to match overall report summary: " <> unmatched

    pure
      OverallReport
        { _producedWithVersion = version,
          _reportResult = reportResult,
          _dependencyReports = dependencyReports,
          _isTruncated = isTruncated
        }
