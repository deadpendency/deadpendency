{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Assessment.DependencyAssessmentWarning () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentViolation ()
import Common.Model.Assessment.DependencyAssessmentWarning
import Lucid
import Text.HTML.TagSoup qualified as T

instance ToHtmlReportBody DependencyAssessmentWarning where
  toHtmlReportBody warning =
    li_ [id_ "warning"] $
      "\x26A0 "
        <> toHtmlReportBody (warning ^. #_violation)

instance FromHtmlReportBody DependencyAssessmentWarning where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        textWarning = stripPrefix "\x26A0 " $ T.renderTags tags
     in DependencyAssessmentWarning <$> fromHtmlReportBody textWarning
