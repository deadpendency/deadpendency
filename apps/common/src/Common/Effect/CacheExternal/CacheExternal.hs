{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.CacheExternal.CacheExternal
  ( loadFromCache,
    loadFromCacheSingle,
    storeInCache,
    storeInCacheSingle,
    CacheExternal (..),
  )
where

import Control.Effect.TH
import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Vector.NonEmpty qualified as NV

type ExpireInSeconds = Integer

type CacheKey = Text

data CacheExternal (t :: Type) (m :: Type -> Type) k where
  LoadFromCache :: (ToJSON t, FromJSON t) => NV.NonEmptyVector CacheKey -> CacheExternal t m (HM.HashMap CacheKey t)
  LoadFromCacheSingle :: (ToJSON t, FromJSON t) => CacheKey -> CacheExternal t m (Maybe t)
  StoreInCache :: (ToJSON t) => ExpireInSeconds -> HM.HashMap CacheKey t -> CacheExternal t m ()
  StoreInCacheSingle :: (ToJSON t) => ExpireInSeconds -> CacheKey -> t -> CacheExternal t m ()

makeSmartConstructors ''CacheExternal
