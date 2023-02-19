{-# LANGUAGE NoImplicitPrelude #-}

module Common.Vector.NonEmptyVector
  ( sortNV,
    concatMaybeNV,
    ordNubNV,
  )
where

import Common.Vector.Vector (concatMaybeV, ordNubV, sortV)
import Data.Function
import Data.Maybe (Maybe)
import Data.Ord
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

-- flattenNVVectors :: NV.NonEmptyVector (V.Vector a) -> V.Vector a
-- flattenNVVectors = NV.foldl' (V.++) V.empty

-- flattenVecNvVectors :: V.Vector (NV.NonEmptyVector a) -> V.Vector a
-- flattenVecNvVectors = V.foldl' (\acc a -> acc V.++ NV.toVector a) V.empty

sortNV :: (Ord a) => NV.NonEmptyVector a -> NV.NonEmptyVector a
sortNV = NV.unsafeFromVector . sortV . NV.toVector

concatMaybeNV :: NV.NonEmptyVector (Maybe a) -> V.Vector a
concatMaybeNV = concatMaybeV . NV.toVector

ordNubNV :: forall a. (Ord a) => NV.NonEmptyVector a -> NV.NonEmptyVector a
ordNubNV = NV.unsafeFromVector . ordNubV . NV.toVector
