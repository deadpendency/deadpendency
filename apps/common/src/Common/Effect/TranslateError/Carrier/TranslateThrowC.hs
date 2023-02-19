module Common.Effect.TranslateError.Carrier.TranslateThrowC (TranslateThrowC (..)) where

import Common.Model.Error.ToAppError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw (..), throwError)

newtype TranslateThrowC (e :: Type) (s :: Type) m a = TranslateThrowC {runTranslateThrowC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Has (Throw s) sig m, ToAppError e s) => Algebra (Throw e :+: sig) (TranslateThrowC e s m) where
  alg hdl sig ctx = case sig of
    (L (Throw error')) -> throwError @s (toAppError error')
    (R other) -> TranslateThrowC $ alg (runTranslateThrowC . hdl) other ctx
