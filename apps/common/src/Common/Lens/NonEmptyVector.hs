{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Lens.NonEmptyVector () where

import Control.Applicative
import Control.Lens
import Data.Bool
import Data.Function
import Data.Int
import Data.Ord
import Data.Vector.NonEmpty qualified as NV
import Relude.Unsafe qualified as Unsafe

type instance Index (NV.NonEmptyVector a) = Int

type instance IxValue (NV.NonEmptyVector a) = a

instance Ixed (NV.NonEmptyVector a) where
  ix i f v
    | 0 <= i && i < NV.length v = f (v NV.! i) <&> \a -> v NV.// [(i, a)]
    | otherwise = pure v
  {-# INLINE ix #-}

instance FunctorWithIndex Int NV.NonEmptyVector where
  imap = NV.imap
  {-# INLINE imap #-}

instance FoldableWithIndex Int NV.NonEmptyVector where
  ifoldr = NV.ifoldr
  {-# INLINE ifoldr #-}
  ifoldl = NV.ifoldl . flip
  {-# INLINE ifoldl #-}
  ifoldr' = NV.ifoldr'
  {-# INLINE ifoldr' #-}
  ifoldl' = NV.ifoldl' . flip
  {-# INLINE ifoldl' #-}

instance TraversableWithIndex Int NV.NonEmptyVector where
  itraverse f v =
    let !n = NV.length v in Unsafe.fromJust . NV.fromListN n <$> itraverse f (NV.toList v)
  {-# INLINE itraverse #-}
