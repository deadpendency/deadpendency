{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Error.ProcessingError () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.HtmlReportTextual
import Common.HtmlReport.Instances.Error.UserError ()
import Common.Model.Error.ProcessingError
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lucid

instance ToHtmlReport ProcessingError where
  toHtmlReport processingError =
    let body = fromLazy . renderText $ toHtmlReportBody processingError
        summary =
          case processingError of
            ProcessingErrorApplication -> "The Deadpendency check has failed to run."
            ProcessingErrorUser _ -> "Your Deadpendency check request was considered invalid."
        title = "Deadpendency Check Result"
     in HtmlReportTextual
          { _title = title,
            _summary = summary,
            _body = body
          }

appError :: Html ()
appError = do
  h3_ "We Experienced an Error! Sorry! \x1F62D"
  p_ "This error has been reported and will be investigated."
  h4_ "What Caused The Error?"
  p_ $ do
    " Either.."
    ul_ $ do
      li_ "a bug with Deadpendency has occurred. Oops!"
      li_ $ a_ [href_ "https://www.githubstatus.com"] "GitHub" <> " or a package registry are experiencing an outage."
  h4_ "What To Do"
  p_ "Once the issue is resolved, the report will automatically re-run."
  p_ $ "If Deadpendency is being impacted by an outage, you can track resolution via " <> a_ [href_ "https://twitter.com/deadpendency"] "twitter" <> "."
  p_ $ "Feel free to " <> a_ [href_ "https://github.com/deadpendency/deadpendency/issues"] "open an issue" <> " if you want more direct feedback, particularly if you think this is a Deadpendency bug."

instance ToHtmlReportBody ProcessingError where
  toHtmlReportBody processingError =
    case processingError of
      ProcessingErrorApplication -> appError
      ProcessingErrorUser userError -> toHtmlReportBody userError

instance FromHtmlReport ProcessingError where
  fromHtmlReport (HtmlReportTextual _ summary body) =
    let htmlDecodedBody = fromLazy $ toLazyText $ htmlEncodedText body
     in case summary of
          "The Deadpendency check has failed to run." -> Right ProcessingErrorApplication
          "Your Deadpendency check request was considered invalid." ->
            ProcessingErrorUser <$> fromHtmlReportBody htmlDecodedBody
          unmatched -> Left . HtmlReportDecodeError $ "Unknown ProcessingError : " <> unmatched
