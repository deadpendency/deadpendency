{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.Effect.GenerateReport where

import CommonTest.Gen.Model.Report
import Hedgehog
import RG.Effect.GenerateReport.Model.GenerateReportResult

genGenerateReportResult :: Gen GenerateReportResult
genGenerateReportResult = GenerateReportResult <$> genOverallReport
