{-# LANGUAGE NoImplicitPrelude #-}

module Common.Util
  ( maybesToEither,
    (<<&>>),
  )
where

import Data.Either
import Data.Functor
import Data.Maybe
import GHC.Base (flip, (.))

maybesToEither :: l -> [Maybe r] -> Either l [r]
maybesToEither _ [] = Right []
maybesToEither l (Nothing : _) = Left l
maybesToEither l ((Just r) : xs) = (:) r <$> maybesToEither l xs

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (fmap . fmap)
{-# INLINE (<<&>>) #-}
