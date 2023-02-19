module Common.GitHub.Internal.InstallationAuthResponse
  ( InstallationAuthResponse (..),
  )
where

import Data.Aeson (FromJSON (..), withObject, (.:))

data InstallationAuthResponse = InstallationAuthResponse
  { _iarToken :: Text,
    _iarExpiresAt :: UTCTime
  }
  deriving stock (Show, Generic)

instance FromJSON InstallationAuthResponse where
  parseJSON =
    withObject "InstallationAuthResponse" $
      \v ->
        InstallationAuthResponse
          <$> v
            .: "token"
          <*> v
            .: "expires_at"
