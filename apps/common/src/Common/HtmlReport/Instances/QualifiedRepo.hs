{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.QualifiedRepo () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.Instances.Internal
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Parsing.RepoParsing
import Lucid
import Text.HTML.TagSoup qualified as T
import Text.Megaparsec qualified as M

instance ToHtmlReportBody QualifiedRepo where
  toHtmlReportBody qr@QualifiedRepo {_repoHost = repoHost} =
    let url = qrToHref qr
        (idText, emojiText, titleText) =
          case repoHost of
            GitHub -> ("repo-link-github", "\x1F4D6", "source repository")
            Bitbucket -> ("repo-link-bitbucket", "\x1F4D5", "Bitbucket repos not currently analyzed")
            GitLab -> ("repo-link-gitlab", "\x1F4D5", "GitLab repos not currently analyzed")
     in a_ [href_ url, title_ titleText, id_ idText] emojiText

instance FromHtmlReportBody QualifiedRepo where
  fromHtmlReportBody input = do
    let tags = T.parseTags input
    hrefText <-
      case tags of
        (t : _) -> Right $ T.fromAttrib "href" t
        _ -> Left . HtmlReportDecodeError $ "unexpected no tags for qualified repo: " <> input
    let eitherResult = M.parse parserQualifiedRepo "QualifiedRepo Report Body" hrefText
    first (\e -> HtmlReportDecodeError $ "Failed to parse URL for qualified repo: " <> show @Text e) eitherResult
