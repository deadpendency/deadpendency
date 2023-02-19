{-# LANGUAGE TemplateHaskell #-}

module RG.Effect.GenerateReport.GenerateReport
  ( GenerateReport (..),
    generateReport,
  )
where

import Control.Effect.TH
import RG.Effect.GenerateReport.Model.GenerateReportRequest
import RG.Effect.GenerateReport.Model.GenerateReportResult

data GenerateReport (m :: Type -> Type) k where
  GenerateReport :: GenerateReportRequest -> GenerateReport m GenerateReportResult

makeSmartConstructors ''GenerateReport
