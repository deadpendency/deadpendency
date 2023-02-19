module DD.Effect.DetermineDependencies.Model.DetermineDependenciesResult
  ( DetermineDependenciesResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicRepoDependencies
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Data.Aeson

data DetermineDependenciesResult = DetermineDependenciesResult
  { _basicRepoDependencies :: BasicRepoDependencies,
    _ignoredRepoDependencies :: IgnoredRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DetermineDependenciesResult where
  toJSON = genericToJSON cleanJSONOptions
