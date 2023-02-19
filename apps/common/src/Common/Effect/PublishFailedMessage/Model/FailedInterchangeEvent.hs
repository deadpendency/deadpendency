module Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
  ( FailedInterchangeEvent (..),
  )
where

import Common.Model.InterchangeEvent.InterchangeEvent
import Data.Aeson (ToJSON)
import GHC.Show qualified as S

data FailedInterchangeEvent p where
  FailedInterchangeEvent :: (ToJSON p) => InterchangeEvent p -> FailedInterchangeEvent p

instance (Eq p) => Eq (FailedInterchangeEvent p) where
  (==) (FailedInterchangeEvent p) (FailedInterchangeEvent p') = p == p'

instance (S.Show p) => S.Show (FailedInterchangeEvent p) where
  show (FailedInterchangeEvent p) = "FailedInterchangeEvent " <> S.show p
