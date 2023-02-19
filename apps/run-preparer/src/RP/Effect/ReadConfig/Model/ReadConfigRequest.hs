module RP.Effect.ReadConfig.Model.ReadConfigRequest
  ( ReadConfigRequest (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Data.Aeson

data ReadConfigRequest = ReadConfigRequest
  { _gitSha :: GitSha,
    _gHQualifiedRepo :: QualifiedRepo
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReadConfigRequest where
  toJSON = genericToJSON cleanJSONOptions
