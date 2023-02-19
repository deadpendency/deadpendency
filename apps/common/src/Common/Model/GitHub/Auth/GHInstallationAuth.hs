module Common.Model.GitHub.Auth.GHInstallationAuth
  ( GHInstallationAuth (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.GitHub.GHAppInstallationId
import Data.Aeson

data GHInstallationAuth = GHInstallationAuth
  { _installationId :: GHAppInstallationId,
    _token :: Text,
    _expirationTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GHInstallationAuth where
  toJSON (GHInstallationAuth installationId _ expirationTime) =
    object
      [ "installationId" .= installationId,
        "expirationTime" .= expirationTime
      ]
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON GHInstallationAuth where
  parseJSON = genericParseJSON cleanJSONOptions
