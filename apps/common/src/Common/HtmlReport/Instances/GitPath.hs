{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.GitPath () where

import Common.HtmlReport.HtmlReport
import Common.Model.Git.GitPath
import Lucid

instance ToHtmlReportBody GitPath where
  toHtmlReportBody (GitPath path) = toHtml path

instance FromHtmlReportBody GitPath where
  fromHtmlReportBody path = Right $ GitPath path
