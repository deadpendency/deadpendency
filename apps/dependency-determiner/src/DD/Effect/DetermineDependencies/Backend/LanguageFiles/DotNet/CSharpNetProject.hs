module DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.CSharpNetProject
  ( CSharpNetProjectInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.DotNetInternal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V

newtype CSharpNetProject = CSharpNetProject
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype CSharpNetProjectInput = CSharpNetProjectInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance CanDetermineDependencies CSharpNetProjectInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile DotNetCSharpProject gitPath)
      _pjBasicDependencies
      . fmap CSharpNetProject
      . parseProjectDepsFromText CSharpNet
      . _pjText
