module DD.Effect.DetermineDependencies.Backend.Model.LanguageDependencyFiles
  ( LanguageDependencyFiles (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.File.DependenciesFileLoad
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

newtype LanguageDependencyFiles = LanguageDependencyFiles
  { _files :: NV.NonEmptyVector DependenciesFileLoad
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LanguageDependencyFiles where
  toJSON = genericToJSON cleanJSONOptions
