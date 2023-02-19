module Common.Model.GitHub.GHRepoOwnerType
  ( GHRepoOwnerType (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data GHRepoOwnerType
  = GHROTUser
  | GHROTOrganization
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON GHRepoOwnerType where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHRepoOwnerType where
  parseJSON = genericParseJSON cleanJSONOptions
