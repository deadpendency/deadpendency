{-# LANGUAGE TemplateHaskell #-}

module RF.Effect.FinishRun.FinishRun
  ( FinishRun (..),
    finishRun,
  )
where

import Control.Effect.TH
import RF.Effect.FinishRun.Model.FinishRunRequest
import RF.Effect.FinishRun.Model.FinishRunResult

data FinishRun (m :: Type -> Type) k where
  FinishRun :: FinishRunRequest -> FinishRun m FinishRunResult

makeSmartConstructors ''FinishRun
