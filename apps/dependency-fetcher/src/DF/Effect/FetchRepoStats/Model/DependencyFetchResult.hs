module DF.Effect.FetchRepoStats.Model.DependencyFetchResult (DependencyFetchResult (..), partitionResultEither) where

import Common.Aeson.Aeson
import Common.Model.Dependency.Enriched.EnrichedDependency
import Common.Model.Dependency.Errored.ErroredDependency
import Data.Aeson

data DependencyFetchResult
  = DFRSuccess EnrichedDependency
  | DFRErrored ErroredDependency
  deriving stock (Eq, Show, Generic)

partitionResultEither :: DependencyFetchResult -> Either ErroredDependency EnrichedDependency
partitionResultEither (DFRSuccess ed) = Right ed
partitionResultEither (DFRErrored fd) = Left fd

instance ToJSON DependencyFetchResult where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions
