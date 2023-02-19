module Common.Effect.PublishComponentResult.Model.ComponentResult
  ( ComponentResult (..),
  )
where

import Common.Model.Error.ProcessingError
import Data.Aeson (ToJSON)
import GHC.Show qualified as S

data ComponentResult p where
  SuccessComponentResult :: (ToJSON p) => p -> ComponentResult p
  FailureComponentResult :: ProcessingError -> ComponentResult p

instance (Eq p) => Eq (ComponentResult p) where
  (==) (SuccessComponentResult p) (SuccessComponentResult p') = p == p'
  (==) (FailureComponentResult f) (FailureComponentResult f') = f == f'
  (==) _ _ = False

instance (S.Show p) => S.Show (ComponentResult p) where
  show (SuccessComponentResult p) = "SuccessComponentResult " <> S.show p
  show (FailureComponentResult f) = "FailureComponentResult " <> S.show f
