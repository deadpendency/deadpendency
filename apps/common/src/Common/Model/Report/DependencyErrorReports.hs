{-# LANGUAGE DataKinds #-}

module Common.Model.Report.DependencyErrorReports
  ( DependencyErrorReports (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Report.DependencyErrorReason
import Common.Model.Report.DependencyErrorReport
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data DependencyErrorReports = DependencyErrorReports
  { _erroredReason :: DependencyErrorReason,
    _errorReports :: NV.NonEmptyVector DependencyErrorReport
  }
  deriving stock (Eq, Show, Generic)

instance Ord DependencyErrorReports where
  compare (DependencyErrorReports reason _) (DependencyErrorReports reason' _) = compare reason reason'

instance ToJSON DependencyErrorReports where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyErrorReports where
  parseJSON = genericParseJSON cleanJSONOptions
