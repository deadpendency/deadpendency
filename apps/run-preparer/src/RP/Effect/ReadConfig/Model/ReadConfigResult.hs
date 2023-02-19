module RP.Effect.ReadConfig.Model.ReadConfigResult
  ( ReadConfigResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.RepoConfig.RepoConfig
import Data.Aeson

newtype ReadConfigResult = ReadConfigResult
  { _repoConfig :: RepoConfig
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReadConfigResult where
  toJSON = genericToJSON cleanJSONOptions
