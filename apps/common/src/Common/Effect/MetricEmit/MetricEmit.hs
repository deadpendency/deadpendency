{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.MetricEmit.MetricEmit
  ( MetricEmit (..),
    metricEmit,
  )
where

import Common.Effect.MetricEmit.Model.MetricEvent
import Control.Effect.TH

data MetricEmit (m :: Type -> Type) k where
  MetricEmit :: MetricEvent -> MetricEmit m ()

makeSmartConstructors ''MetricEmit
