{-# LANGUAGE TemplateHaskell #-}

module RP.Effect.ReadConfig.ReadConfig
  ( ReadConfig (..),
    configRead,
  )
where

import Control.Effect.TH
import RP.Effect.ReadConfig.Model.ReadConfigRequest
import RP.Effect.ReadConfig.Model.ReadConfigResult

data ReadConfig (m :: Type -> Type) k where
  ConfigRead :: ReadConfigRequest -> ReadConfig m ReadConfigResult

makeSmartConstructors ''ReadConfig
