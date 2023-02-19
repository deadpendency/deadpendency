module Common.Model.Report.DependencyIgnoreReport
  ( DependencyIgnoreReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Data.Aeson

newtype DependencyIgnoreReport = DependencyIgnoreReport
  { _dependencyIdentifier :: DependencyIdentifier
  }
  deriving stock (Eq, Show, Generic)

instance Ord DependencyIgnoreReport where
  compare (DependencyIgnoreReport di) (DependencyIgnoreReport di') =
    compare di di'

instance ToJSON DependencyIgnoreReport where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyIgnoreReport where
  parseJSON = genericParseJSON cleanJSONOptions
