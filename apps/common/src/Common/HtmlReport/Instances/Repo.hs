{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Repo () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.QualifiedRepo ()
import Common.Model.Git.Repo
import Common.Parsing.RepoParsing
import Lucid
import Text.HTML.TagSoup qualified as T
import Text.Megaparsec qualified as M
import Text.URI qualified as URI

instance ToHtmlReportBody Repo where
  toHtmlReportBody (RepoQR qr) = toHtmlReportBody qr
  toHtmlReportBody (RepoUnknown uri) =
    let url = fromLazy . renderText . toHtml $ URI.render uri
     in a_ [href_ url, title_ "Non-GitHub repostiory was not analyzed", id_ "repo-link-other"] "\x1F4D5"

instance FromHtmlReportBody Repo where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
    (linkId, hrefText) <-
      case tags of
        (t : _) -> Right (T.fromAttrib "id" t, T.fromAttrib "href" t)
        _ -> Left . HtmlReportDecodeError $ "unexpected missing tags for repo: " <> input
    case linkId of
      "repo-link-other" -> do
        let eitherResult = M.parse parserRepoUnknown "Repo Report Body" hrefText
        first (\e -> HtmlReportDecodeError $ "Failed to parse URL for repo: " <> show @Text e) eitherResult
      _ -> RepoQR <$> fromHtmlReportBody input
