{-# LANGUAGE NoImplicitPrelude #-}

module Common.These
  ( thisMaybeThese,
    thatMaybeThese,
    theseFromMaybe,
    maybesToThese,
  )
where

import Data.Either (Either (..))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.These

thisMaybeThese :: a -> Maybe b -> These a b
thisMaybeThese a Nothing = This a
thisMaybeThese a (Just b) = These a b

thatMaybeThese :: Maybe a -> b -> These a b
thatMaybeThese Nothing b = That b
thatMaybeThese (Just a) b = These a b

theseFromMaybe :: e -> Maybe a -> Maybe b -> Either e (These a b)
theseFromMaybe _ (Just a) maybeB = Right $ thisMaybeThese a maybeB
theseFromMaybe _ maybeA (Just b) = Right $ thatMaybeThese maybeA b
theseFromMaybe e Nothing Nothing = Left e

maybesToThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybesToThese (Just a) (Just b) = Just $ These a b
maybesToThese (Just a) Nothing = Just $ This a
maybesToThese Nothing (Just b) = Just $ That b
maybesToThese Nothing Nothing = Nothing
