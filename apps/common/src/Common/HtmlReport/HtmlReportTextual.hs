module Common.HtmlReport.HtmlReportTextual
  ( HtmlReportTextual (..),
  )
where

data HtmlReportTextual = HtmlReportTextual
  { _title :: Text,
    _summary :: Text,
    _body :: Text
  }
  deriving stock (Eq, Show, Generic)
