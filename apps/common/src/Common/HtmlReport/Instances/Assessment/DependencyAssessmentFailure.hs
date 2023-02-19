{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Assessment.DependencyAssessmentFailure () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.Instances.Assessment.DependencyAssessmentViolation ()
import Common.Model.Assessment.DependencyAssessmentFailure
import Lucid
import Text.HTML.TagSoup qualified as T

instance ToHtmlReportBody DependencyAssessmentFailure where
  toHtmlReportBody failure =
    li_ [id_ "failure"] $
      "\x274C "
        <> toHtmlReportBody (failure ^. #_violation)

instance FromHtmlReportBody DependencyAssessmentFailure where
  fromHtmlReportBody input =
    let tags = T.parseTags input
        textFailure = stripPrefix "\x274C " $ T.renderTags tags
     in DependencyAssessmentFailure <$> fromHtmlReportBody textFailure
