{-# LANGUAGE TemplateHaskell #-}

module SR.Effect.RunSmokeTest.RunSmokeTest
  ( RunSmokeTest (..),
    runSmokeTest,
  )
where

import Control.Effect.TH
import Data.Vector qualified as V
import SR.Effect.RunSmokeTest.Model.SmokeResult

data RunSmokeTest (m :: Type -> Type) k where
  RunSmokeTest :: RunSmokeTest m (V.Vector SmokeResult)

makeSmartConstructors ''RunSmokeTest
