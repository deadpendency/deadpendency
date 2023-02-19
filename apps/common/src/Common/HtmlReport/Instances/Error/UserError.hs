{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Error.UserError () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.DependenciesFileType ()
import Common.HtmlReport.Instances.DependencyLanguageReport ()
import Common.HtmlReport.Instances.GitPath ()
import Common.HtmlReport.Instances.Vector ()
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Error.UserError
import Common.Model.Git.GitPath
import Common.Parsing.Megaparsec
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Lucid
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

noDeps :: Html ()
noDeps = do
  h3_ "We were unable to find any dependencies \x1F914"
  h4_ "How Deadpendency Finds Dependencies"
  ol_ $ do
    li_ "Determine the top 2 languages used in a repository (as reported by GitHub)."
    li_ $ "Search the source 1 directory deep for " <> a_ [href_ "https://deadpendency.com/language-support"] "supported dependency files" <> " of those languages."
  h4_ "What To Do"
  p_ $ "You can " <> a_ [href_ "https://deadpendency.com/docs/config#additional-dependency-files"] "add additional dependency files via Deadpendency config" <> "."
  p_ $ "If you think this is a bug, or your programming language / dependency file format is not supported, why not " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> "?"

tooManyDepFiles :: Int -> Html ()
tooManyDepFiles count = do
  h3_ "We found too many dependency files \x1F635"
  p_ $ "Deadpendency cannot currently " <> a_ [href_ "https://deadpendency.com/docs/troubleshooting#too-many-dependency-files"] "assess more than 100 dependency files" <> ". Your repository contains " <> show count <> " dependency files."
  p_ $ "If you have a use case that requires this many dependency files, feel free to " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> "."

tooManyDeps :: Int -> Html ()
tooManyDeps count = do
  h3_ "We found too many dependencies \x1F635"
  p_ $ "Deadpendency cannot currently " <> a_ [href_ "https://deadpendency.com/docs/troubleshooting#too-many-dependencies"] "assess more than 500 unique dependencies" <> ". Your repository contains " <> show count <> " unique dependencies."
  p_ $ "If you have a use case that requires this many dependencies, feel free to " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> "."

missingDepFile :: GitPath -> Html ()
missingDepFile filePath = do
  h3_ "Missing Dependency File \x1F9D0"
  p_ "You specified to load a dependency file via config, but it could not be found."
  h4_ "Details"
  ul_ $ do
    li_ $ "File Path: " <> code_ (toHtmlReportBody filePath)
  h4_ "What To Do"
  p_ "Please add the dependency file, or remove it from your Deadpendency config."
  p_ $ "If you believe this is a bug, please " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> "."

invalidDepFile :: DependenciesFileType -> GitPath -> Text -> Html ()
invalidDepFile fileType path errorMessage = do
  h3_ "The following dependency file is invalid \x1F914"
  h4_ "Details"
  ul_ $ do
    li_ $ "File Path: " <> code_ (toHtmlReportBody path)
    li_ $ "File Type: " <> code_ (toHtmlReportBody fileType)
    li_ $ "Error Message: " <> code_ (toHtml errorMessage)
  h4_ "What To Do"
  p_ "Please fix the dependency file and push a new commit."
  p_ $ "If you believe the dependency file is in fact valid, please " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> "."

unableToParseDepFile :: DependenciesFileType -> GitPath -> Text -> Html ()
unableToParseDepFile fileType path errorMessage = do
  h3_ "We were unable to load a dependency file \x1F914"
  h4_ "Details"
  ul_ $ do
    li_ $ "File Path: " <> code_ (toHtmlReportBody path)
    li_ $ "File Type: " <> code_ (toHtmlReportBody fileType)
    li_ $ "Error Message: " <> code_ (toHtml errorMessage)
  h4_ "What To Do"
  p_ "This is likely a failure with Deadpendency, sorry! \x1F647"
  p_ $ "It has been flagged and will be investigated. Once the issue has been resolved, the report will automatically re-run. Feel free to " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> " if you want to directly track the resolution of the problem."
  p_ "If the dependency file is in fact invalid, pushing a fix to the file will resolve the issue."

instance ToHtmlReportBody UserError where
  toHtmlReportBody =
    \case
      UserErrorInvalidConfig invalidReason -> "Deadpendency config was invalid. Reason: " <> toHtml invalidReason
      UserErrorIgnoredAllDependencies -> "All found dependencies were ignored."
      UserErrorNoDependenciesFound -> noDeps
      UserErrorTooManyDependencyFiles count -> tooManyDepFiles count
      UserErrorTooManyDependencies count -> tooManyDeps count
      UserErrorInvalidDependencyFile file path error' -> invalidDepFile file path error'
      UserErrorUnableToParseDependencyFile file path error' -> unableToParseDepFile file path error'
      UserErrorUserSpecificedMissingFile gitPath -> missingDepFile gitPath
      UserErrorAllDepsFailedFetch nvDependencyLanguageReports -> do
        "All dependencies failed analysis. See below for the failures."
        ul_ $
          toHtmlReportBody $
            NV.toVector nvDependencyLanguageReports

instance FromHtmlReportBody UserError where
  fromHtmlReportBody input =
    let parsers =
          ( M.try parseInvalidConfig
              <|> M.try parseIgnoredAllDependencies
              <|> M.try parseNoDependenciesFound
              <|> M.try parseTooManyDependencyFiles
              <|> M.try parseTooManyDependencies
              <|> M.try parseInvalidDependencyFile
              <|> M.try parseUnableToParseDependencyFile
              <|> M.try parseMissingDependencyFile
              <|> parseAllDepsFailed
          )
        maybeErrorResult = mParseMaybe parsers input
     in maybeToRight
          (HtmlReportDecodeError $ "Unknown UserError: " <> input)
          maybeErrorResult

parseInvalidConfig :: MParser UserError
parseInvalidConfig = do
  M.string "Deadpendency config was invalid. Reason: "
  reason <- pack <$> M.someTill M.anySingle (void M.eol <|> M.eof)
  pure $ UserErrorInvalidConfig reason

parseIgnoredAllDependencies :: MParser UserError
parseIgnoredAllDependencies =
  M.string "All found dependencies were ignored." $> UserErrorIgnoredAllDependencies

parseNoDependenciesFound :: MParser UserError
parseNoDependenciesFound = M.string "<h3>We were unable to find any dependencies" $> UserErrorNoDependenciesFound

parseTooManyDependencyFiles :: MParser UserError
parseTooManyDependencyFiles = do
  M.string "<h3>We found too many dependency files"
  M.skipManyTill M.anySingle (M.string "Your repository contains ")
  count <- L.decimal
  pure $ UserErrorTooManyDependencies count

parseTooManyDependencies :: MParser UserError
parseTooManyDependencies = do
  M.string "<h3>We found too many dependencies"
  M.skipManyTill M.anySingle (M.string "Your repository contains ")
  count <- L.decimal
  pure $ UserErrorTooManyDependencies count

parseInvalidDependencyFile :: MParser UserError
parseInvalidDependencyFile = do
  M.string "<h3>The following dependency file is invalid"
  M.skipManyTill M.anySingle (M.string "File Path: <code>")
  filePathAsText <- pack <$> M.someTill M.anySingle (M.string "</code>")
  M.skipManyTill M.anySingle (M.string "File Type: <code>")
  fileTypeAsText <- pack <$> M.someTill M.anySingle (M.string "</code>")
  M.skipManyTill M.anySingle (M.string "Error Message: <code>")
  error' <- pack <$> M.someTill M.anySingle (M.string "</code>")
  case (fromHtmlReportBody filePathAsText, fromHtmlReportBody fileTypeAsText) of
    (Right filePath, Right fileType) -> pure $ UserErrorInvalidDependencyFile fileType filePath error'
    _ -> fail "Failure to parse dependency file type"

parseUnableToParseDependencyFile :: MParser UserError
parseUnableToParseDependencyFile = do
  M.string "<h3>We were unable to load a dependency file"
  M.skipManyTill M.anySingle (M.string "File Path: <code>")
  filePathAsText <- pack <$> M.someTill M.anySingle (M.string "</code>")
  M.skipManyTill M.anySingle (M.string "File Type: <code>")
  fileTypeAsText <- pack <$> M.someTill M.anySingle (M.string "</code>")
  M.skipManyTill M.anySingle (M.string "Error Message: <code>")
  error' <- pack <$> M.someTill M.anySingle (M.string "</code>")
  case (fromHtmlReportBody filePathAsText, fromHtmlReportBody fileTypeAsText) of
    (Right filePath, Right fileType) -> pure $ UserErrorUnableToParseDependencyFile fileType filePath error'
    _ -> fail "Failure to parse dependency file type"

parseMissingDependencyFile :: MParser UserError
parseMissingDependencyFile = do
  M.string "<h3>Missing Dependency File"
  M.skipManyTill M.anySingle (M.string "File Path: <code>")
  filePathAsText <- pack <$> M.someTill M.anySingle (M.string "</code>")
  case fromHtmlReportBody filePathAsText of
    Right filePath -> pure $ UserErrorUserSpecificedMissingFile filePath
    _ -> fail "Failure to parse dependency file type"

parseAllDepsFailed :: MParser UserError
parseAllDepsFailed = do
  M.string "All dependencies failed analysis. See below for the failures."
  errorReportsText <- pack <$> M.someTill M.anySingle M.eof
  let eitherFailureReports = fromHtmlReportBody errorReportsText
  case eitherFailureReports of
    Right languageReports ->
      if V.null languageReports
        then M.fancyFailure (one . M.ErrorFail $ "Parse all failed with no reports found: " <> unpack errorReportsText)
        else pure $ UserErrorAllDepsFailedFetch $ NV.unsafeFromVector languageReports
    Left e -> M.fancyFailure (one . M.ErrorFail $ "Failure to parse failure report: " <> show @String e)
