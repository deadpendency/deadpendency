module Common.Model.GitHub.Auth.GHSharedAppAuth
  ( GHSharedAppAuth (..),
  )
where

import Data.Aeson

data GHSharedAppAuth = GHSharedAppAuth
  { _jwtToken :: Text,
    _expiryTime :: UTCTime
  }
  deriving stock (Show, Generic)

instance ToJSON GHSharedAppAuth where
  toJSON (GHSharedAppAuth _ expiryTime) =
    object
      [ "expiryTime" .= expiryTime
      ]
