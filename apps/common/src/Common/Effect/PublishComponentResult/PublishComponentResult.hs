{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.PublishComponentResult.PublishComponentResult
  ( PublishComponentResult (..),
    publishComponentResult,
  )
where

import Common.Effect.PublishComponentResult.Model.ComponentResult
import Control.Effect.TH

data PublishComponentResult (p :: Type) (m :: Type -> Type) k where
  PublishComponentResult :: ComponentResult p -> PublishComponentResult p m ()

makeSmartConstructors ''PublishComponentResult
