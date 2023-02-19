{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Error.DependencyErrorReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.Error.DependencyErrorReason ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.Model.Report.DependencyErrorReport
import Data.Vector qualified as V
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} ToHtmlReportBody (V.Vector DependencyErrorReport) where
  toHtmlReportBody reports =
    if V.null reports
      then mempty
      else do
        ul_ $
          foldMap' toHtmlReportBody reports

instance {-# OVERLAPPING #-} FromHtmlReportBody (V.Vector DependencyErrorReport) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecLiTags = V.fromList $ splitByLiTags tags
     in traverse (fromHtmlReportBody . T.renderTags) vecLiTags

instance ToHtmlReportBody DependencyErrorReport where
  toHtmlReportBody (DependencyErrorReport dependencyIdentifier maybePackageLink) =
    let dependencyIdentifierHtml = toHtmlReportBody dependencyIdentifier
        linksPrefix = toHtmlRaw (" &ndash; " :: Text)
        linksHtml =
          case maybePackageLink of
            Just packageLink ->
              linksPrefix <> toHtmlReportBody packageLink
            Nothing -> mempty
     in li_ $
          dependencyIdentifierHtml
            <> linksHtml

instance FromHtmlReportBody DependencyErrorReport where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        identifierHtmlAsText = T.renderTags $ take 3 $ drop 1 tags

    let packageTags = take 3 $ dropWhile (T.~/= ("<a id=package-repo-link>" :: String)) tags
    maybePackageLink <-
      case packageTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    dependencyIdentifier <- fromHtmlReportBody identifierHtmlAsText
    pure
      DependencyErrorReport
        { _dependencyIdentifier = dependencyIdentifier,
          _dependencyPackageLink = maybePackageLink
        }
