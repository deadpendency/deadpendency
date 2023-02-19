{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Config where

import Common.Model.Config.AppVersion
import Hedgehog

genAppVersion :: Gen AppVersion
genAppVersion = pure $ AppVersion "1.1.1"
