module Common.Model.Dependency.Enriched.EnrichedRepoDependencies
  ( EnrichedRepoDependencies (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Enriched.EnrichedDependency
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

newtype EnrichedRepoDependencies = EnrichedRepoDependencies
  { _dependencies :: NV.NonEmptyVector EnrichedDependency
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EnrichedRepoDependencies where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON EnrichedRepoDependencies where
  parseJSON = genericParseJSON cleanJSONOptions
