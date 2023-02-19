{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyFailureReport () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentFailure ()
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentWarning ()
import Common.HtmlReport.Instances.DependencyReportIdentifier ()
import Common.HtmlReport.Instances.Internal
import Common.HtmlReport.Instances.PackageLink ()
import Common.HtmlReport.Instances.Repo ()
import Common.HtmlReport.Instances.Vector ()
import Common.Model.Report.DependencyFailureReport
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Lucid
import Text.HTML.TagSoup qualified as T

instance {-# OVERLAPPING #-} ToHtmlReportBody (V.Vector DependencyFailureReport) where
  toHtmlReportBody reports =
    if V.null reports
      then mempty
      else do
        ul_ $
          foldMap' toHtmlReportBody reports

instance {-# OVERLAPPING #-} FromHtmlReportBody (V.Vector DependencyFailureReport) where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        vecLiTags = V.fromList $ splitByLiTags tags
     in traverse (fromHtmlReportBody . T.renderTags) vecLiTags

instance ToHtmlReportBody DependencyFailureReport where
  toHtmlReportBody (DependencyFailureReport dependencyIdentifier maybeRepo maybePackageLink warnings failures) =
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
        collatedWarningsFailures =
          ul_ $
            toHtmlReportBody failures <> toHtmlReportBody warnings
     in li_ $
          dependencyIdentifierHtml
            <> linksHtml
            <> collatedWarningsFailures

instance FromHtmlReportBody DependencyFailureReport where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
        identifierAsHtml = T.renderTags $ take 3 $ drop 1 $ dropWhile (not . T.isTagOpenName "strong") tags
    dependencyIdentifier <- fromHtmlReportBody identifierAsHtml

    let idPortionTags = takeWhile (not . T.isTagOpenName "ul") tags

        repoTags = take 2 $ dropWhile (not . tagHasIdPrefix "a" "repo-link") idPortionTags
    maybeRepo <-
      case repoTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    let packageTags = take 3 $ dropWhile (T.~/= ("<a id=package-repo-link>" :: String)) idPortionTags
    maybePackageLink <-
      case packageTags of
        t@(_ : _) -> Just <$> fromHtmlReportBody (T.renderTags t)
        _ -> pure Nothing

    let failureWarningTags = dropWhile (not . T.isTagOpenName "ul") tags
    warnings <- (traverse (fromHtmlReportBody . getTagInnerContentText "li") . V.fromList . filteredLiFromId "warning") failureWarningTags
    failures <- (traverse (fromHtmlReportBody . getTagInnerContentText "li") . V.fromList . filteredLiFromId "failure") failureWarningTags
    nvFailures <- maybeToRight (HtmlReportDecodeError input) $ NV.fromVector failures
    pure
      DependencyFailureReport
        { _dependencyIdentifier = dependencyIdentifier,
          _dependencyPackageLink = maybePackageLink,
          _dependencyRepo = maybeRepo,
          _dependencyAssessmentWarnings = warnings,
          _dependencyAssessmentFailures = nvFailures
        }
