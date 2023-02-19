module DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
  ( CanDetermineDependencies (..),
    CDDWrapper (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V

class CanDetermineDependencies a where
  determineDependencies :: GitPath -> a -> Either DetermineDependenciesError (V.Vector BasicDependency)

data CDDWrapper where
  CDDWrapper :: (CanDetermineDependencies a) => GitPath -> a -> CDDWrapper
