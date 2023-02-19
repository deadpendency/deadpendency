{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.PublishSimpleResult.PublishSimpleResult
  ( PublishSimpleResult (..),
    publishSimpleResult,
  )
where

import Common.Effect.PublishSimpleResult.Model.SimpleResult
import Control.Effect.TH

data PublishSimpleResult (p :: Type) (m :: Type -> Type) k where
  PublishSimpleResult :: SimpleResult p -> PublishSimpleResult p m ()

makeSmartConstructors ''PublishSimpleResult
