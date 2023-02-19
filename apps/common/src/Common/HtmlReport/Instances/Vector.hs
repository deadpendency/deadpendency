{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HtmlReport.Instances.Vector
  (
  )
where

import Common.HtmlReport.HtmlReport
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

instance {-# OVERLAPPABLE #-} (ToHtmlReportBody a) => ToHtmlReportBody (NV.NonEmptyVector a) where
  toHtmlReportBody = toHtmlReportBody . NV.toVector

instance {-# OVERLAPPABLE #-} (ToHtmlReportBody a) => ToHtmlReportBody (V.Vector a) where
  toHtmlReportBody items
    | V.null items = mempty
    | otherwise =
        foldMap' toHtmlReportBody items
