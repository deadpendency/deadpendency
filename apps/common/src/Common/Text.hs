{-# LANGUAGE NoImplicitPrelude #-}

module Common.Text
  ( stripPrefix,
    stripSuffix,
  )
where

import Data.Text qualified as T
import Relude.Monad.Reexport (fromMaybe)

stripPrefix :: T.Text -> T.Text -> T.Text
stripPrefix toStrip input =
  fromMaybe input (T.stripPrefix toStrip input)

stripSuffix :: T.Text -> T.Text -> T.Text
stripSuffix toStrip input =
  fromMaybe input (T.stripSuffix toStrip input)

-- emptyToMaybe :: T.Text -> Maybe T.Text
-- emptyToMaybe x
--   | T.null x = Nothing
--   | otherwise = Just x
