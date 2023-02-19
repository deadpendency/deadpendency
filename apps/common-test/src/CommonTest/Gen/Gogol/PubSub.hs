{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Gogol.PubSub where

import Hedgehog
import Network.Google.PubSub qualified as G

genPubSubReceivedMessage :: Gen G.ReceivedMessage
genPubSubReceivedMessage = pure G.receivedMessage
