module Common.Model.RepoConfig.IgnoreDependenciesConfig
  ( IgnoreDependenciesConfig (..),
    IgnoreLanguageDependencies (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

data IgnoreDependenciesConfig
  = IDAll (V.Vector DependencyName)
  | IDSpecific (V.Vector IgnoreLanguageDependencies)
  deriving stock (Eq, Show, Generic)

data IgnoreLanguageDependencies = IgnoreLanguageDependencies
  { _programmingLanguage :: ProgrammingLanguage,
    _dependencies :: NV.NonEmptyVector DependencyName
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON IgnoreDependenciesConfig where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON IgnoreDependenciesConfig where
  parseJSON = genericParseJSON cleanJSONOptions

instance ToJSON IgnoreLanguageDependencies where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON IgnoreLanguageDependencies where
  parseJSON = genericParseJSON cleanJSONOptions
