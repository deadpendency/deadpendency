{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Git.RepoOwner
  ( RepoOwner (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype RepoOwner = RepoOwner
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance ToJSON RepoOwner where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RepoOwner where
  parseJSON = genericParseJSON cleanJSONOptions
