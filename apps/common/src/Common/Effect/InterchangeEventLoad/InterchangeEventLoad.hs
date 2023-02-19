{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.InterchangeEventLoad.InterchangeEventLoad
  ( loadInterchangeEvent,
    InterchangeEventLoad (..),
  )
where

import Common.Model.InterchangeEvent.InterchangeEvent
import Control.Effect.TH
import Network.Google.PubSub qualified as G

data InterchangeEventLoad (t :: Type) (m :: Type -> Type) k where
  LoadInterchangeEvent :: G.ReceivedMessage -> InterchangeEventLoad t m (InterchangeEvent t)

makeSmartConstructors ''InterchangeEventLoad
