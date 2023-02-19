module Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadMockC
  ( mockInterchangeEventLoad,
  )
where

import Common.Effect.InterchangeEventLoad.InterchangeEventLoad (InterchangeEventLoad (..))
import Common.Model.InterchangeEvent.InterchangeEvent
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.Reader (ask)

newtype InterchangeEventLoadMockC (t :: Type) m a = InterchangeEventLoadMockC {runInterchangeEventLoadMockC :: ReaderC (InterchangeEvent t) m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (InterchangeEventLoad t :+: sig) (InterchangeEventLoadMockC t m) where
  alg hdl sig ctx = case sig of
    (L (LoadInterchangeEvent _)) -> InterchangeEventLoadMockC $ ask @(InterchangeEvent t) <&> (<$ ctx)
    (R other) -> InterchangeEventLoadMockC $ alg (runInterchangeEventLoadMockC . hdl) (R other) ctx

mockInterchangeEventLoad :: InterchangeEvent t -> InterchangeEventLoadMockC t m a -> m a
mockInterchangeEventLoad t = runReader t . runInterchangeEventLoadMockC
