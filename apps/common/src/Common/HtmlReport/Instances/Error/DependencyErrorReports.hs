{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Error.DependencyErrorReports () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.Error.DependencyErrorReason ()
import Common.HtmlReport.Instances.Error.DependencyErrorReport ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.Model.Report.DependencyErrorReports
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} FromHtmlReportBody (V.Vector DependencyErrorReports) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecErrorReportsTags = V.fromList $ partitionsFromIdByTag "div" "dependency-report-errors" tags
     in traverse (fromHtmlReportBody . T.renderTags) vecErrorReportsTags

instance ToHtmlReportBody DependencyErrorReports where
  toHtmlReportBody (DependencyErrorReports errorReason errorReports) =
    let errorReasonHtml = toHtmlReportBody errorReason
        errorReportsHtml = foldMap' toHtmlReportBody errorReports
     in div_ [id_ "dependency-report-errors"] $ do
          h5_ errorReasonHtml
          ul_ errorReportsHtml

instance FromHtmlReportBody DependencyErrorReports where
  fromHtmlReportBody input = do
    let tags = T.parseTags input

    erroredReason <- fromHtmlReportBody $ T.innerText $ getTagInnerContent "h5" tags
    vErrorReports <- traverse (fromHtmlReportBody . T.renderTags) $ splitByLiTags tags
    case NV.fromList vErrorReports of
      Just nvErrorReports ->
        pure
          DependencyErrorReports
            { _erroredReason = erroredReason,
              _errorReports = nvErrorReports
            }
      Nothing ->
        Left $ HtmlReportDecodeError $ "Unexpected missing error reports: " <> input
