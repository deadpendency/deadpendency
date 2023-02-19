{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Parsing.HsYAML
  (
  )
where

import Data.Vector qualified as V
import Data.YAML

instance (FromYAML a) => FromYAML (V.Vector a) where
  parseYAML = withSeq "!!seq" (V.mapM parseYAML . V.fromList)
