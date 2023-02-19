module RG.Effect.AssessDependencies.Backend.Rules.Rule
  ( Rule,
  )
where

import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.Git.Repo
import Common.Model.RepoConfig.Rules.RulesConfig
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment

type Rule = UTCTime -> RulesConfig -> Maybe Repo -> These DependencyRegistryInfo DependencyRepoStats -> InternalDependencyAssessment
