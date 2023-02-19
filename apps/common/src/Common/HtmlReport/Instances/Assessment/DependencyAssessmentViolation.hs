{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Assessment.DependencyAssessmentViolation (parsePackageDeprecated, parseDeprecatedFor) where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.PackageLink
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.Registry
import Common.Model.Report.PackageLink
import Common.Parsing.Megaparsec
import Data.Vector qualified as V
import Lucid
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as M

instance ToHtmlReportBody DependencyAssessmentViolation where
  toHtmlReportBody =
    \case
      DAVNoRecentCommits errorAtMonths maybeLastCommitTime ->
        toHtml $ "No commits within the last " <> show @Text errorAtMonths <> " months." <> maybe "" ((<>) " Last commit: " . pack . formatTime defaultTimeLocale "%Y-%m-%d") maybeLastCommitTime
      DAVFewYearlyCommits errorAtCount commitCount -> toHtml $ "Only " <> show @Text commitCount <> " commit(s) within the last year. Expected more than " <> show @Text errorAtCount <> "."
      DAVNoRecentPackageRelease errorAtMonths lastReleaseDate -> toHtml $ "Last package release over " <> show @Text errorAtMonths <> " months ago. Last release: " <> pack (formatTime defaultTimeLocale "%Y-%m-%d" lastReleaseDate)
      DAVRepoArchived -> "Repository is archived."
      DAVPackageDeprecated registry deprecationType maybeDeprecationMessage deprecatedForNames -> packageDeprecated registry deprecationType maybeDeprecationMessage deprecatedForNames
      DAVIsFork -> "The repository is a fork."
      DAVSingleRecentAuthor -> "All commits within the last year were authored by a single person."
      DAVRepoNotIdentified -> "A source repository was not identified."
      DAVRepoNotFound -> "Source repository was not found."

packageDeprecated :: Registry -> DAVDeprecationType -> Maybe Text -> V.Vector DependencyName -> Html ()
packageDeprecated registry davDeprecationType maybeDeprecationMessage deprecatedForNames =
  let prefixHtml = toHtml @Text "Package has been"
      deprecationTypeHtml =
        toHtml @Text $
          case davDeprecationType of
            DAVDTDeprecated -> "deprecated"
            DAVDTAbandoned -> "flagged abandonded"
            DAVDTRelocated -> "relocated"
      registryHtml = toHtml $ registryAsText registry
      hasMoreInfo = isJust maybeDeprecationMessage
      moreInfoHtml = case maybeDeprecationMessage of
        Just deprecationMessage -> li_ $ "More Info: " <> i_ (toHtml deprecationMessage)
        Nothing -> mempty
      packageLinks = PackageLink registry <$> deprecatedForNames
      hasDeprecatedFor = not (V.null deprecatedForNames)
      deprecatedForHtml =
        if hasDeprecatedFor
          then li_ $ "Recommended Replacement(s): " <> fold (intersperseV ", " (fmap (renderPackageLinkWithName False) packageLinks))
          else mempty
      additionalInfoList =
        if hasMoreInfo || hasDeprecatedFor
          then ul_ (moreInfoHtml <> deprecatedForHtml)
          else mempty
   in prefixHtml <> " " <> deprecationTypeHtml <> " in " <> registryHtml <> "." <> additionalInfoList

instance FromHtmlReportBody DependencyAssessmentViolation where
  fromHtmlReportBody input =
    let parsers =
          ( M.try parseNoRecentCommits
              <|> M.try parseFewYearlyCommits
              <|> M.try parseRepoArchived
              <|> M.try parsePackageDeprecated
              <|> M.try parseNoRecentPackageRelease
              <|> M.try parseRepoIsAFork
              <|> M.try parseRepoNotIdentified
              <|> M.try parseRepoNotFound
              <|> parseSingleRecentAuthor
          )
        maybeFailureResult = mParseMaybe (M.try parsers) input
     in maybeToRight
          (HtmlReportDecodeError $ "Unable to match violation: " <> input)
          maybeFailureResult

parseNoRecentCommits :: MParser DependencyAssessmentViolation
parseNoRecentCommits = do
  M.string "No commits within the last "
  errorAtMonths <- M.decimal
  M.string " months."
  maybeLastCommitDateTime <- M.optional $ do
    _ <- M.string " Last commit: "
    M.some M.anySingle >>= parseTimeM False defaultTimeLocale "%Y-%m-%d"
  case maybeLastCommitDateTime of
    Just lastCommitDateTime -> pure $ DAVNoRecentCommits errorAtMonths (Just lastCommitDateTime)
    Nothing -> pure $ DAVNoRecentCommits errorAtMonths Nothing

parseFewYearlyCommits :: MParser DependencyAssessmentViolation
parseFewYearlyCommits = do
  M.string "Only "
  commitCount <- M.decimal
  M.string " commit(s) within the last year. Expected more than "
  errorAtCount <- M.decimal
  M.char '.'
  pure $ DAVFewYearlyCommits errorAtCount commitCount

parseRepoArchived :: MParser DependencyAssessmentViolation
parseRepoArchived = M.string "Repository is archived." $> DAVRepoArchived

parseRepoNotIdentified :: MParser DependencyAssessmentViolation
parseRepoNotIdentified = M.string "A source repository was not identified." $> DAVRepoNotIdentified

parseRepoNotFound :: MParser DependencyAssessmentViolation
parseRepoNotFound = M.string "Source repository was not found." $> DAVRepoNotFound

parsePackageDeprecated :: MParser DependencyAssessmentViolation
parsePackageDeprecated = do
  M.string "Package has been "
  davDeprecationType <-
    (M.string "deprecated" $> DAVDTDeprecated)
      <|> (M.string "flagged abandonded" $> DAVDTAbandoned)
      <|> (M.string "relocated" $> DAVDTRelocated)
  M.string " in "
  registry <- registryParser <* M.char '.'
  maybeDeprecationMessage <- M.optional $ fmap pack $ M.try $ M.skipManyTill M.anySingle (M.string "More Info: <i>") *> M.someTill M.anySingle (M.string "</i>")
  deprecatedForNames <- fmap (fromMaybe V.empty) $ M.optional $ M.try $ M.skipManyTill M.anySingle (M.string "Recommended Replacement(s): ") *> parseDeprecatedFor
  pure $ DAVPackageDeprecated registry davDeprecationType maybeDeprecationMessage deprecatedForNames

parseDeprecatedFor :: MParser (V.Vector DependencyName)
parseDeprecatedFor = do
  names <- _dependencyName <<$>> M.someTill (parserRegistryAnchor <* M.optional (M.string ", ")) (M.string "</li>")
  pure $
    V.fromList names

parseNoRecentPackageRelease :: MParser DependencyAssessmentViolation
parseNoRecentPackageRelease = do
  M.string "Last package release over "
  errorAtMonths <- M.decimal
  M.string " months ago. Last release: "
  lastReleaseDateTime <- M.some M.anySingle >>= parseTimeM False defaultTimeLocale "%Y-%m-%d"
  pure $ DAVNoRecentPackageRelease errorAtMonths lastReleaseDateTime

parseRepoIsAFork :: MParser DependencyAssessmentViolation
parseRepoIsAFork = M.string "The repository is a fork." $> DAVIsFork

parseSingleRecentAuthor :: MParser DependencyAssessmentViolation
parseSingleRecentAuthor = M.string "All commits within the last year were authored by a single person." $> DAVSingleRecentAuthor
