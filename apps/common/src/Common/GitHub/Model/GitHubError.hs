{-# LANGUAGE DeriveAnyClass #-}

module Common.GitHub.Model.GitHubError
  ( GitHubError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.ConsideredAppFailure
import Data.Aeson

data GitHubError
  = GHEExceptionalError Text
  | GHEReponseParseError Text
  | GHEAbuseDetectedError Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ConsideredAppFailure GitHubError where
  consideredAppFailure =
    \case
      _ -> True

instance ToJSON GitHubError where
  toJSON = genericToJSON cleanJSONOptions
