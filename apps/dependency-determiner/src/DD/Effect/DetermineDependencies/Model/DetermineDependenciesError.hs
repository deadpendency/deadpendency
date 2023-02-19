module DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
  ( DetermineDependenciesError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Error.ConsideredAppFailure
import Common.Model.Error.ProcessingError
import Common.Model.Error.ToProcessingError
import Common.Model.Error.UserError
import Common.Model.Git.GitPath
import Data.Aeson

data DetermineDependenciesError
  = NoDependenciesFound
  | AllDependenciesIgnored
  | TooManyDependencyFiles Int
  | TooManyDependencies Int
  | DependencyFileInvalid DependenciesFileType GitPath Text -- we are confident in our parsing logic, this is just a simple user error
  | UnableToParseDependencyFile DependenciesFileType GitPath Text -- we are not confident, this will trigger a failure that can easily be replayed.
  | UserSpecificedMissingFile GitPath
  | DependencyFilesMismatch Text
  deriving stock (Eq, Show, Generic)

instance ToProcessingError DetermineDependenciesError where
  toProcessingError =
    \case
      NoDependenciesFound -> Just $ ProcessingErrorUser UserErrorNoDependenciesFound
      AllDependenciesIgnored -> Just $ ProcessingErrorUser UserErrorIgnoredAllDependencies
      TooManyDependencyFiles count -> Just $ ProcessingErrorUser $ UserErrorTooManyDependencyFiles count
      TooManyDependencies count -> Just $ ProcessingErrorUser $ UserErrorTooManyDependencies count
      DependencyFileInvalid file path error' -> Just $ ProcessingErrorUser $ UserErrorInvalidDependencyFile file path error'
      UnableToParseDependencyFile file path error' -> Just $ ProcessingErrorUser $ UserErrorUnableToParseDependencyFile file path error'
      UserSpecificedMissingFile path -> Just $ ProcessingErrorUser $ UserErrorUserSpecificedMissingFile path
      DependencyFilesMismatch _ -> Nothing

instance ConsideredAppFailure DetermineDependenciesError where
  consideredAppFailure =
    \case
      NoDependenciesFound -> False
      AllDependenciesIgnored -> False
      TooManyDependencyFiles _ -> False
      TooManyDependencies _ -> False
      DependencyFileInvalid {} -> False
      UnableToParseDependencyFile {} -> True
      UserSpecificedMissingFile _ -> False
      DependencyFilesMismatch _ -> True

instance ToJSON DetermineDependenciesError where
  toJSON = genericToJSON cleanJSONOptions
