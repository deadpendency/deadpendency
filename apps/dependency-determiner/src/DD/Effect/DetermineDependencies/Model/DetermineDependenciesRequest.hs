module DD.Effect.DetermineDependencies.Model.DetermineDependenciesRequest
  ( DetermineDependenciesRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import Data.Aeson
import Data.Vector qualified as V

data DetermineDependenciesRequest = DetermineDependenciesRequest
  { _programmingLanguages :: V.Vector ProgrammingLanguage,
    _qualifiedRepo :: QualifiedRepo,
    _commitSha :: GitSha,
    _additionalDependencies :: V.Vector BasicDependency,
    _ignoreDependenciesConfig :: IgnoreDependenciesConfig,
    _additionalDependencyFiles :: V.Vector DependenciesFileLoad
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DetermineDependenciesRequest where
  toJSON = genericToJSON cleanJSONOptions
