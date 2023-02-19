{-# LANGUAGE NoImplicitPrelude #-}

module Common.Vector.Vector
  ( safeMaximumV,
    fromMaybeV,
    intersperseV,
    safeHeadV,
    safeLastV,
    sortV,
    prependToAllV,
    ordNubV,
    noDupV,
    concatMaybeV,
  )
where

import Control.Applicative
import Data.Bool
import Data.Function
import Data.Maybe (Maybe (..), maybe)
import Data.Ord
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as A
import Relude.Base (Eq)

concatMaybeV :: V.Vector (Maybe a) -> V.Vector a
concatMaybeV =
  V.foldl' (\acc a -> maybe acc (acc `V.snoc`) a) V.empty

safeMaximumV :: (Ord a) => V.Vector a -> Maybe a
safeMaximumV vec
  | V.null vec = Nothing
  | otherwise = Just $ V.maximum vec

fromMaybeV :: Maybe a -> V.Vector a
fromMaybeV (Just a) = V.singleton a
fromMaybeV Nothing = V.empty

safeHeadV :: V.Vector a -> Maybe a
safeHeadV vec
  | V.null vec = Nothing
  | otherwise = Just $ V.unsafeHead vec

safeLastV :: V.Vector a -> Maybe a
safeLastV vec
  | V.null vec = Nothing
  | otherwise = Just $ V.unsafeLast vec

intersperseV :: a -> V.Vector a -> V.Vector a
intersperseV sep vec
  | V.null vec = vec
  | otherwise = V.unsafeHead vec `V.cons` prependToAllV sep (V.unsafeTail vec)

prependToAllV :: a -> V.Vector a -> V.Vector a
prependToAllV sep vec
  | V.null vec = vec
  | otherwise = sep `V.cons` (V.unsafeHead vec `V.cons` prependToAllV sep (V.unsafeTail vec))

sortV :: (Ord a) => V.Vector a -> V.Vector a
sortV vec = V.create $ do
  mvec <- V.thaw vec
  A.sort mvec
  pure mvec

ordNubV :: forall a. (Ord a) => V.Vector a -> V.Vector a
ordNubV = go Set.empty
  where
    go :: Set.Set a -> V.Vector a -> V.Vector a
    go theSet newVec
      | V.null newVec = V.empty
      | otherwise =
          let x = V.unsafeHead newVec
              xs = V.unsafeTail newVec
           in if x `Set.member` theSet
                then go theSet xs
                else x `V.cons` go (Set.insert x theSet) xs

noDupV :: forall a. (Eq a) => V.Vector a -> V.Vector a
noDupV = go V.empty
  where
    go :: V.Vector a -> V.Vector a -> V.Vector a
    go newVec toAddVec
      | V.null toAddVec = newVec
      | otherwise =
          let x = V.unsafeHead toAddVec
              xs = V.unsafeTail toAddVec
           in if V.notElem x newVec
                then go (newVec `V.snoc` x) xs
                else go newVec xs
