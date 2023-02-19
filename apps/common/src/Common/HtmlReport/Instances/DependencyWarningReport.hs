{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyWarningReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentWarning ()
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.HtmlReport.Instances.Repo ()
import Common.HtmlReport.Instances.Vector ()
import Common.Model.Report.DependencyWarningReport
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} ToHtmlReportBody (V.Vector DependencyWarningReport) where
  toHtmlReportBody reports =
    if V.null reports
      then mempty
      else do
        ul_ $
          foldMap' toHtmlReportBody reports

instance {-# OVERLAPPING #-} FromHtmlReportBody (V.Vector DependencyWarningReport) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecLiTags = V.fromList $ splitByLiTags tags
     in traverse (fromHtmlReportBody . T.renderTags) vecLiTags

instance ToHtmlReportBody DependencyWarningReport where
  toHtmlReportBody (DependencyWarningReport dependencyIdentifier maybeRepo maybePackageLink warnings) =
    let dependencyIdentifierHtml = strong_ $ toHtmlReportBody dependencyIdentifier
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
        collatedWarningsErrors =
          ul_ $
            toHtmlReportBody warnings
     in li_ $
          dependencyIdentifierHtml
            <> linksHtml
            <> collatedWarningsErrors

instance FromHtmlReportBody DependencyWarningReport where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        identifierHtmlAsText = T.renderTags $ take 3 $ drop 1 $ dropWhile (not . T.isTagOpenName "strong") tags
    dependencyIdentifier <- fromHtmlReportBody identifierHtmlAsText

    let idPortionTags = takeWhile (not . T.isTagOpenName "ul") tags

        repoTags = take 3 $ dropWhile (not . tagHasIdPrefix "a" "repo-link") idPortionTags
    maybeRepo <-
      case repoTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    let packageTags = take 3 $ dropWhile (T.~/= ("<a id=package-repo-link>" :: String)) idPortionTags
    maybePackageLink <-
      case packageTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    let errorWarningTags = dropWhile (not . T.isTagOpenName "ul") tags
    warnings <- (traverse (fromHtmlReportBody . getTagInnerContentText "li") . V.fromList . splitByLiTags) errorWarningTags
    nvWarnings <- maybeToRight (HtmlReportDecodeError "Parsed no warnings for warning dep") $ NV.fromVector warnings
    pure
      DependencyWarningReport
        { _dependencyIdentifier = dependencyIdentifier,
          _dependencyPackageLink = maybePackageLink,
          _dependencyRepo = maybeRepo,
          _dependencyAssessmentWarnings = nvWarnings
        }
