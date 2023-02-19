{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyPassReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.HtmlReport.Instances.Repo ()
import Common.Model.Report.DependencyPassReport
import Data.Vector qualified as V
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} ToHtmlReportBody (V.Vector DependencyPassReport) where
  toHtmlReportBody reports =
    if V.null reports
      then mempty
      else do
        ul_ $
          foldMap' toHtmlReportBody reports

instance {-# OVERLAPPING #-} FromHtmlReportBody (V.Vector DependencyPassReport) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecLiTags = V.fromList $ splitByLiTags tags
     in traverse (fromHtmlReportBody . T.renderTags) vecLiTags

instance ToHtmlReportBody DependencyPassReport where
  toHtmlReportBody (DependencyPassReport dependencyIdentifier maybeRepo maybePackageLink) =
    let dependencyIdentifierHtml = toHtmlReportBody dependencyIdentifier

        linksPrefix = toHtmlRaw (" &ndash; " :: Text)
        linksHtml =
          case (maybeRepo, maybePackageLink) of
            (Just repo, Just packageLink) ->
              linksPrefix <> toHtmlReportBody repo <> " " <> toHtmlReportBody packageLink
            (Just repo, Nothing) ->
              linksPrefix <> toHtmlReportBody repo
            (Nothing, Just packageLink) ->
              linksPrefix <> toHtmlReportBody packageLink
            (Nothing, Nothing) -> mempty
     in li_ $
          dependencyIdentifierHtml
            <> linksHtml

instance FromHtmlReportBody DependencyPassReport where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        identifierAsHtml = T.renderTags $ take 3 $ drop 1 tags
    dependencyIdentifier <- fromHtmlReportBody identifierAsHtml

    let repoTags = take 3 $ dropWhile (not . tagHasIdPrefix "a" "repo-link") tags
    maybeRepo <-
      case repoTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    let packageTags = take 3 $ dropWhile (T.~/= ("<a id=package-repo-link>" :: String)) tags
    maybePackageLink <-
      case packageTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    pure
      DependencyPassReport
        { _dependencyIdentifier = dependencyIdentifier,
          _dependencyRepo = maybeRepo,
          _dependencyPackageLink = maybePackageLink
        }
