module Common.Model.RepoConfig.RepoConfig
  ( RepoConfig (..),
    defaultRepoConfig,
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.RepoConfig.FileLoadPlan
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import Common.Model.RepoConfig.Rules.RulesConfig
  ( RulesConfig,
    defaultRulesConfig,
  )
import Data.Aeson
import Data.Vector qualified as V

data RepoConfig = RepoConfig
  { _additionalDependencies :: V.Vector BasicDependency,
    _ignoreDependenciesConfig :: IgnoreDependenciesConfig,
    _additionalDependencyFiles :: V.Vector DependenciesFileLoad,
    _fileLoadPlan :: FileLoadPlan,
    _rulesConfig :: RulesConfig
  }
  deriving stock (Eq, Show, Generic)

defaultRepoConfig :: RepoConfig
defaultRepoConfig =
  RepoConfig
    { _additionalDependencies = V.empty,
      _ignoreDependenciesConfig = IDSpecific V.empty,
      _additionalDependencyFiles = V.empty,
      _fileLoadPlan = defaultFileLoadPlan,
      _rulesConfig = defaultRulesConfig
    }

instance ToJSON RepoConfig where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RepoConfig where
  parseJSON = genericParseJSON cleanJSONOptions
