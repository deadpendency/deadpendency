module Common.Effect.PublishSimpleResult.Model.SimpleResult
  ( SimpleResult (..),
  )
where

import Data.Aeson (ToJSON)
import GHC.Show qualified as S

data SimpleResult p where
  SimpleResult :: (ToJSON p) => p -> SimpleResult p

instance (Eq p) => Eq (SimpleResult p) where
  (==) (SimpleResult p) (SimpleResult p') = p == p'

instance (S.Show p) => S.Show (SimpleResult p) where
  show (SimpleResult p) = "SimpleResult " <> S.show p
