module DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.VisualBasicNetProject
  ( VisualBasicNetProjectInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.DotNet.DotNetInternal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V

newtype VisualBasicNetProject = VisualBasicNetProject
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype VisualBasicNetProjectInput = VisualBasicNetProjectInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance CanDetermineDependencies VisualBasicNetProjectInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile DotNetVisualBasicProject gitPath)
      _pjBasicDependencies
      . fmap VisualBasicNetProject
      . parseProjectDepsFromText VisualBasicNet
      . _pjText
