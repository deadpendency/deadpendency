module Common.Model.Dependency.Ignored.IgnoredRepoDependencies
  ( IgnoredRepoDependencies (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Ignored.IgnoredDependency
import Data.Aeson
import Data.Vector qualified as V

newtype IgnoredRepoDependencies = IgnoredRepoDependencies
  { _dependencies :: V.Vector IgnoredDependency
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON IgnoredRepoDependencies where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON IgnoredRepoDependencies where
  parseJSON = genericParseJSON cleanJSONOptions
