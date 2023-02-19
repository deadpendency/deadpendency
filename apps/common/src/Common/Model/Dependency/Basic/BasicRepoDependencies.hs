module Common.Model.Dependency.Basic.BasicRepoDependencies
  ( BasicRepoDependencies (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Basic.BasicDependency
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

newtype BasicRepoDependencies = BasicRepoDependencies
  { _dependencies :: NV.NonEmptyVector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BasicRepoDependencies where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON BasicRepoDependencies where
  parseJSON = genericParseJSON cleanJSONOptions
