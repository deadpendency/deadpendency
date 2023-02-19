module Common.HtmlReport.HtmlReportDecodeError
  ( HtmlReportDecodeError (..),
  )
where

newtype HtmlReportDecodeError = HtmlReportDecodeError {_ntText :: Text}
  deriving stock (Eq, Show)
