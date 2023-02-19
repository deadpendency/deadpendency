{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.DependencyReportIdentifier () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.Internal
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import Common.Parsing.RepoParsing
import Lucid
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

instance ToHtmlReportBody DependencyIdentifier where
  toHtmlReportBody =
    \case
      (DependencyIdentifierNamed (DependencyName name)) -> span_ $ toHtml name
      (DependencyIdentifierRepo repo (Just (DependencyName name))) -> span_ $ toHtml name <> " (" <> renderSimpleRepo repo <> ")"
      (DependencyIdentifierRepo repo _) -> span_ $ "(" <> renderSimpleRepo repo <> ")"

instance FromHtmlReportBody DependencyIdentifier where
  fromHtmlReportBody input =
    let parsers =
          ignoreSpan
            *> ( M.try parseDependencyIdentifierRepo
                   <|> parseDependencyIdentifierNamed
               )
     in maybeToRight
          (HtmlReportDecodeError $ "Unable to parse DependencyIdentifier: " <> input)
          (mParseMaybe parsers input)

ignoreSpan :: MParser ()
ignoreSpan = void $ M.string "<span>"

parseDependencyIdentifierRepo :: MParser DependencyIdentifier
parseDependencyIdentifierRepo = do
  maybeDependencyName <- M.optional $ M.try (parserDependencyName <* M.hspace1)
  repo <- parseQualifiedRepo
  pure $ DependencyIdentifierRepo repo maybeDependencyName

parseDependencyIdentifierNamed :: MParser DependencyIdentifier
parseDependencyIdentifierNamed = DependencyIdentifierNamed <$> parserDependencyName

parseQualifiedRepo :: MParser QualifiedRepo
parseQualifiedRepo = do
  M.char '('
  repoHost <- parserRepoHost
  M.char ':'
  repoOwner <- RepoOwner . pack <$> M.someTill repoNameChar (M.char '/')
  repoName <- RepoName . pack <$> M.someTill repoNameChar (M.char ')')
  pure $ QualifiedRepo repoHost repoOwner repoName

renderSimpleRepo :: QualifiedRepo -> Html ()
renderSimpleRepo (QualifiedRepo repoHost (RepoOwner owner) (RepoName name)) =
  toHtml $ renderSimpleRepoHost repoHost <> ":" <> (owner <> "/" <> name)
