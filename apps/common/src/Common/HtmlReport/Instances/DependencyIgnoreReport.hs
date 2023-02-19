{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyIgnoreReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.Internal
import Common.Model.Report.DependencyIgnoreReport
import Data.Vector qualified as V
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} ToHtmlReportBody (V.Vector DependencyIgnoreReport) where
  toHtmlReportBody ignoreReports =
    if V.null ignoreReports
      then mempty
      else do
        ul_ $
          foldMap' toHtmlReportBody ignoreReports

instance {-# OVERLAPPING #-} FromHtmlReportBody (V.Vector DependencyIgnoreReport) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecLiTags = V.fromList $ splitByLiTags tags
     in traverse (fromHtmlReportBody . T.renderTags) vecLiTags

instance ToHtmlReportBody DependencyIgnoreReport where
  toHtmlReportBody (DependencyIgnoreReport dependencyIdentifier) =
    let dependencyIdentifierHtml = toHtmlReportBody dependencyIdentifier
     in li_ dependencyIdentifierHtml

instance FromHtmlReportBody DependencyIgnoreReport where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        identifierAsHtml = T.renderTags $ drop 1 tags
    dependencyIdentifier <- fromHtmlReportBody identifierAsHtml
    pure
      DependencyIgnoreReport
        { _dependencyIdentifier = dependencyIdentifier
        }
