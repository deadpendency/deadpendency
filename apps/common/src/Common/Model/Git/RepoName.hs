{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Git.RepoName
  ( RepoName (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype RepoName = RepoName
  { _ntText :: Text
  }
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (NFData)

instance ToJSON RepoName where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RepoName where
  parseJSON = genericParseJSON cleanJSONOptions
