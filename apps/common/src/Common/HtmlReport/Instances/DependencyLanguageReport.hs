{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyLanguageReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentFailure ()
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentWarning ()
import Common.HtmlReport.Instances.DependencyFailureReport ()
import Common.HtmlReport.Instances.DependencyIgnoreReport ()
import Common.HtmlReport.Instances.DependencyPassReport ()
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.DependencyWarningReport ()
import Common.HtmlReport.Instances.Error.DependencyErrorReports ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.HtmlReport.Instances.QualifiedRepo ()
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Report.DependencyLanguageReport
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} (FromHtmlReportBody (V.Vector a)) => FromHtmlReportBody (V.Vector (DependencyLanguageReport a)) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecLanguageReportTags = V.fromList $ partitionsFromIdByTag "div" "language-report" tags
     in traverse (fromHtmlReportBody . T.renderTags) vecLanguageReportTags

instance (ToHtmlReportBody (V.Vector a)) => ToHtmlReportBody (DependencyLanguageReport a) where
  toHtmlReportBody languageReport = do
    let programmingLanguage = languageReport ^. #_programmingLanguage
        reports = NV.toVector $ languageReport ^. #_reports
    div_ [id_ "language-report"] $ do
      h4_ $ i_ $ toHtml $ plToPretty programmingLanguage

      toHtmlReportBody reports

instance (FromHtmlReportBody (V.Vector a)) => FromHtmlReportBody (DependencyLanguageReport a) where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        fromHeading = dropWhile (T.~/= ("<h4>" :: String)) tags
        plHeading = T.innerText $ take 5 fromHeading
        minusHeading = drop 5 fromHeading
        -- reportBodyRootTag = join $ partitionsFromIdByTag "div" "dependency-report-errors" fromHeading
        programmingLanguage = plFromPretty plHeading

    reports <- fromHtmlReportBody $ T.renderTags minusHeading
    nvReports <- maybeToRight (HtmlReportDecodeError $ "Unexpected no reports found: " <> input) $ NV.fromVector reports

    pure $
      DependencyLanguageReport
        { _programmingLanguage = programmingLanguage,
          _reports = nvReports
        }
