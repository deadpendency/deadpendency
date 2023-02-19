{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Git.RepoHost
  ( RepoHost (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data RepoHost
  = GitHub
  | Bitbucket
  | GitLab
  deriving stock (Show, Generic, Eq, Enum, Bounded)
  deriving anyclass (NFData)

instance ToJSON RepoHost where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RepoHost where
  parseJSON = genericParseJSON cleanJSONOptions
