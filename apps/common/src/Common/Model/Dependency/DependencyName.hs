{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.DependencyName
  ( DependencyName (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype DependencyName = DependencyName
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance ToJSON DependencyName where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyName where
  parseJSON = genericParseJSON cleanJSONOptions
