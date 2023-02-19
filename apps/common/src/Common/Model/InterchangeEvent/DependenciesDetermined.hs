module Common.Model.InterchangeEvent.DependenciesDetermined
  ( DependenciesDetermined (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicRepoDependencies
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Data.Aeson

data DependenciesDetermined = DependenciesDetermined
  { _basicRepoDependencies :: BasicRepoDependencies,
    _ignoredRepoDependencies :: IgnoredRepoDependencies
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DependenciesDetermined where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependenciesDetermined where
  parseJSON = genericParseJSON cleanJSONOptions
