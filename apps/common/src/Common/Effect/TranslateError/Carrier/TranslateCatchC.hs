module Common.Effect.TranslateError.Carrier.TranslateCatchC (TranslateCatchC (..)) where

import Common.Model.Error.FromAppError
import Control.Algebra
import Control.Effect.Catch

newtype TranslateCatchC (e :: Type) (s :: Type) m a = TranslateCatchC {runTranslateCatchC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Has (Catch s) sig m, FromAppError e s) => Algebra (Catch e :+: sig) (TranslateCatchC e s m) where
  alg hdl sig ctx = case sig of
    (L (Catch m h)) ->
      catchError @s (hdl (m <$ ctx)) (hdl . (<$ ctx) . maybe m h . fromAppError @e @s)
    (R other) -> TranslateCatchC $ alg (runTranslateCatchC . hdl) other ctx
