module DD.Effect.DetermineDependencies.Backend.Model.RawDepFileContent (RawDepFileContent (..)) where

import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.GitHub.GHRepoFile

data RawDepFileContent = RawDepFileContent
  { _depFileType :: DependenciesFileType,
    _fileContent :: GHRepoFile
  }
