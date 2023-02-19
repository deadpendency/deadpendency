{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Error.DependencyErrorReason () where

import Common.HtmlReport.HtmlReport
import Common.HtmlReport.HtmlReportDecodeError
import Common.Model.Report.DependencyErrorReason

instance ToHtmlReportBody DependencyErrorReason where
  toHtmlReportBody =
    \case
      DERRegistryUnexpectedStructure -> "Failed to load from the registry"
      DERNoRegistryOrRepoData -> "Package was missing from the registry"
      DERProcessingFailure -> "Failed to process. This is being investigated."

instance FromHtmlReportBody DependencyErrorReason where
  fromHtmlReportBody =
    \case
      "Failed to load from the registry" -> Right DERRegistryUnexpectedStructure
      "Package was missing from the registry" -> Right DERNoRegistryOrRepoData
      "Failed to process. This is being investigated." -> Right DERProcessingFailure
      unmatched -> Left . HtmlReportDecodeError $ "Unknown DependencyErrorReason: " <> unmatched
