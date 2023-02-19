module Common.Model.Dependency.File.DependenciesFileLoadDetails
  ( DependenciesFileLoadDetails (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitFileMatch
import Common.Model.Git.GitPath
import Data.Aeson

data DependenciesFileLoadDetails
  = DFLDSpecific Text
  | DFLDSearch GitFileMatch
  | DFLDDirectorySearch GitPath GitFileMatch
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON DependenciesFileLoadDetails where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependenciesFileLoadDetails where
  parseJSON = genericParseJSON cleanJSONOptions
