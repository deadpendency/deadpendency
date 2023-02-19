{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Common.Effect.AppEventEmit.Model.AppEventAdditional where

import Data.Aeson
import Text.Show qualified (show)

data AppEventAdditional where
  AppEventAdditional :: (ToJSON a) => a -> AppEventAdditional

instance Eq AppEventAdditional where
  a == b = toJSON a == toJSON b

instance Show AppEventAdditional where
  show = show . toJSON

instance ToJSON AppEventAdditional where
  toJSON (AppEventAdditional a) = toJSON a
