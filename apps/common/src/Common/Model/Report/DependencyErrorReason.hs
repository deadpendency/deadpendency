module Common.Model.Report.DependencyErrorReason
  ( DependencyErrorReason (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data DependencyErrorReason
  = DERRegistryUnexpectedStructure
  | DERNoRegistryOrRepoData
  | DERProcessingFailure
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)

instance ToJSON DependencyErrorReason where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyErrorReason where
  parseJSON = genericParseJSON cleanJSONOptions
