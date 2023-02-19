{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.AppVersion () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.Model.Config.AppVersion
import Common.Parsing.Megaparsec
import Lucid
import Text.HTML.TagSoup qualified as T
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

instance ToHtmlReportBody AppVersion where
  toHtmlReportBody appVersion =
    p_ [id_ "deadpendency-version"] ("Deadpendency Version: " <> toHtml (appVersion ^. #_ntText))

instance FromHtmlReportBody AppVersion where
  fromHtmlReportBody input = do
    let versionText = T.innerText $ T.parseTags input
        eitherResult = M.parse parserAppVersion "AppVersion Report Body" versionText
    first (\e -> HtmlReportDecodeError $ "Failed to parse AppVersion: " <> input <> " error: " <> show @Text e) eitherResult

parserAppVersion :: MParser AppVersion
parserAppVersion = do
  M.string "Deadpendency Version: "
  version <- pack <$> M.some (M.numberChar <|> M.char '.')
  pure $
    AppVersion version
