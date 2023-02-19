module Common.Model.Dependency.File.DependenciesFileType
  ( DependenciesFileType (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data DependenciesFileType
  = PipRequirementsTxt
  | PythonSetupPy
  | PipenvPipfile
  | PyProjectToml
  | NpmPackageJson
  | PackagistComposerJson
  | BundlerGemfile
  | RubyGemsGemspec
  | HpackPackageYaml
  | HaskellCabal
  | CratesCargoToml
  | DotNetCSharpProject
  | DotNetVisualBasicProject
  | MavenPomXml
  | BuildGradle
  | BuildSbt
  | GoMod
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)

instance ToJSON DependenciesFileType where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependenciesFileType where
  parseJSON = genericParseJSON cleanJSONOptions
