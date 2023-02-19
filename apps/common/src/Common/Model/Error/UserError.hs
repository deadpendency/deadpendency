module Common.Model.Error.UserError
  ( UserError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Git.GitPath
import Common.Model.Report.DependencyErrorReports
import Common.Model.Report.DependencyLanguageReport
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data UserError
  = UserErrorInvalidConfig Text
  | UserErrorIgnoredAllDependencies
  | UserErrorNoDependenciesFound
  | UserErrorTooManyDependencyFiles Int
  | UserErrorTooManyDependencies Int
  | UserErrorInvalidDependencyFile DependenciesFileType GitPath Text
  | UserErrorUnableToParseDependencyFile DependenciesFileType GitPath Text
  | UserErrorUserSpecificedMissingFile GitPath
  | UserErrorAllDepsFailedFetch (NV.NonEmptyVector (DependencyLanguageReport DependencyErrorReports))
  deriving stock (Eq, Show, Generic)

instance ToJSON UserError where
  toJSON = genericToJSON cleanJSONOptions

instance FromJSON UserError where
  parseJSON = genericParseJSON cleanJSONOptions
