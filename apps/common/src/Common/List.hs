{-# LANGUAGE NoImplicitPrelude #-}

module Common.List
  ( headMaybe,
  )
where

import Data.Maybe

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a : _) = Just a
