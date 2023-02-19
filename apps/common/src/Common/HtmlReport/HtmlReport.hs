module Common.HtmlReport.HtmlReport
  ( ToHtmlReportBody (..),
    FromHtmlReportBody (..),
    ToHtmlReport (..),
    FromHtmlReport (..),
  )
where

import Common.HtmlReport.HtmlReportDecodeError
import Common.HtmlReport.HtmlReportTextual
import Lucid

class ToHtmlReportBody a where
  toHtmlReportBody :: a -> Html ()

class FromHtmlReportBody a where
  fromHtmlReportBody :: Text -> Either HtmlReportDecodeError a

class (ToHtmlReportBody a) => ToHtmlReport a where
  toHtmlReport :: a -> HtmlReportTextual

class FromHtmlReport a where
  fromHtmlReport :: HtmlReportTextual -> Either HtmlReportDecodeError a
