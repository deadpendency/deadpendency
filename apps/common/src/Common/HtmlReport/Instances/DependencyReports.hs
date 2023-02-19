{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyReports () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentFailure ()
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentWarning ()
import Common.HtmlReport.Instances.DependencyFailureReport ()
import Common.HtmlReport.Instances.DependencyIgnoreReport ()
import Common.HtmlReport.Instances.DependencyLanguageReport ()
import Common.HtmlReport.Instances.DependencyPassReport ()
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.DependencyWarningReport ()
import Common.HtmlReport.Instances.Error.DependencyErrorReports ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.HtmlReport.Instances.QualifiedRepo ()
import Common.HtmlReport.Instances.Vector ()
import Common.Model.Report.DependencyReports
import Data.Vector qualified as V
import Lucid
import Text.HTML.TagSoup qualified as T

instance ToHtmlReportBody DependencyReports where
  toHtmlReportBody (DRSingleLanguageReports passReports warnReports failReports ignoreReports errorReports) =
    renderSingleReports unhealthyTopSection "Unhealthy \x1F9DF" "fail-section" failReports
      <> renderSingleReports mempty "Healthy With Warnings \x1F912" "warn-section" warnReports
      <> renderSingleReports mempty "Healthy \x1F60A" "pass-section" passReports
      <> renderSingleReports mempty "Ignored \x1F636" "ignore-section" ignoreReports
      <> renderMultiReports mempty "Failed To Analyze \x1F61E" "error-section" errorReports
  toHtmlReportBody (DRMultiLanguageReports passLanguageReports warnLanguageReports failLanguageReports ignoreLanguageReports errorLanguageReports) =
    renderMultiReports unhealthyTopSection "Unhealthy \x1F9DF" "fail-section" failLanguageReports
      <> renderMultiReports mempty "Healthy With Warnings \x1F912" "warn-section" warnLanguageReports
      <> renderMultiReports mempty "Healthy \x1F60A" "pass-section" passLanguageReports
      <> renderMultiReports mempty "Ignored \x1F636" "ignore-section" ignoreLanguageReports
      <> renderMultiReports mempty "Failed To Analyze \x1F61E" "error-section" errorLanguageReports

unhealthyTopSection :: Html ()
unhealthyTopSection =
  a_ [href_ "https://deadpendency.com/docs/getting-started#the-check-fails"] "Learn how to handle unhealthy dependencies"

renderSingleReports :: (ToHtmlReportBody (V.Vector a)) => Html () -> Html () -> Text -> V.Vector a -> Html ()
renderSingleReports topSection heading headingId reports =
  if V.null reports
    then mempty
    else div_ [id_ headingId] $ do
      topSection
      h3_ heading
      toHtmlReportBody reports

renderMultiReports :: (ToHtmlReportBody (V.Vector a)) => Html () -> Html () -> Text -> V.Vector a -> Html ()
renderMultiReports topSection heading headingId reports =
  if V.null reports
    then mempty
    else div_ [id_ headingId] $ do
      topSection
      h3_ heading
      toHtmlReportBody reports

instance FromHtmlReportBody DependencyReports where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        isMultiLanguageReport = any (T.isTagOpenName "h4") tags
        passSectionTags = getTagContentWithId "div" "pass-section" tags
        warnSectionTags = getTagContentWithId "div" "warn-section" tags
        failSectionTags = getTagContentWithId "div" "fail-section" tags
        ignoreSectionTags = getTagContentWithId "div" "ignore-section" tags
        errorSectionTags = getTagContentWithId "div" "error-section" tags

    if isMultiLanguageReport
      then do
        passLanguageReports <- fromHtmlReportBody $ T.renderTags passSectionTags
        warnLanguageReports <- fromHtmlReportBody $ T.renderTags warnSectionTags
        failLanguageReports <- fromHtmlReportBody $ T.renderTags failSectionTags
        ignoreLanguageReports <- fromHtmlReportBody $ T.renderTags ignoreSectionTags
        errorLanguageReports <- fromHtmlReportBody $ T.renderTags errorSectionTags

        pure $
          DRMultiLanguageReports passLanguageReports warnLanguageReports failLanguageReports ignoreLanguageReports errorLanguageReports
      else do
        passReports <- fromHtmlReportBody $ T.renderTags passSectionTags
        warningReports <- fromHtmlReportBody $ T.renderTags warnSectionTags
        failureReports <- fromHtmlReportBody $ T.renderTags failSectionTags
        ignoreReports <- fromHtmlReportBody $ T.renderTags ignoreSectionTags
        errorReports <- fromHtmlReportBody $ T.renderTags errorSectionTags

        pure $
          DRSingleLanguageReports passReports warningReports failureReports ignoreReports errorReports
