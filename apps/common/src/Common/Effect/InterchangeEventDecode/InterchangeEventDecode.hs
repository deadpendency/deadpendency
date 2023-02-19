{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.InterchangeEventDecode.InterchangeEventDecode
  ( decodeInterchangeEvent,
    InterchangeEventDecode (..),
  )
where

import Control.Effect.TH
import Network.Google.PubSub qualified as G

data InterchangeEventDecode (t :: Type) (m :: Type -> Type) k where
  DecodeInterchangeEvent :: G.ReceivedMessage -> InterchangeEventDecode t m t

makeSmartConstructors ''InterchangeEventDecode
