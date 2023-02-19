module Common.Model.Dependency.Enriched.EnrichedDependency
  ( EnrichedDependency (..),
    getRepoFromEnriched,
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.Repo
import Data.Aeson

data EnrichedDependency = EnrichedDependency
  { _programmingLanguage :: ProgrammingLanguage,
    _dependencyIdentifier :: DependencyIdentifier,
    _dependencyType :: Maybe DependencyType,
    _data :: These DependencyRegistryInfo DependencyRepoStats
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EnrichedDependency where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON EnrichedDependency where
  parseJSON = genericParseJSON cleanJSONOptions

getRepoFromEnriched :: EnrichedDependency -> Maybe Repo
getRepoFromEnriched ed =
  (RepoQR <$> getDIRepo (ed ^. #_dependencyIdentifier)) <|> (ed ^? (#_data . here . #_sourceRepo . _Just))
