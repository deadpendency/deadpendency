module Common.Model.Report.DependencyLanguageReport
  ( DependencyLanguageReport (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data DependencyLanguageReport reportType = DependencyLanguageReport
  { _programmingLanguage :: ProgrammingLanguage,
    _reports :: NV.NonEmptyVector reportType
  }
  deriving stock (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (DependencyLanguageReport a) where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance (FromJSON a) => FromJSON (DependencyLanguageReport a) where
  parseJSON = genericParseJSON cleanJSONOptions
