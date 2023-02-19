module Common.Effect.InterchangeEventDecode.Carrier.InterchangeEventDecodeMockC
  ( InterchangeEventDecodeMockC (..),
    mockInterchangeEventDecode,
  )
where

import Common.Effect.InterchangeEventDecode.InterchangeEventDecode (InterchangeEventDecode (..))
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.Reader (ask)

newtype InterchangeEventDecodeMockC (t :: Type) m a = InterchangeEventDecodeMockC {runInterchangeEventDecodeMockC :: ReaderC t m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (InterchangeEventDecode t :+: sig) (InterchangeEventDecodeMockC t m) where
  alg hdl sig ctx = case sig of
    (L (DecodeInterchangeEvent _)) -> InterchangeEventDecodeMockC $ ask @t >>= pure . (<$ ctx)
    (R other) -> InterchangeEventDecodeMockC $ alg (runInterchangeEventDecodeMockC . hdl) (R other) ctx

mockInterchangeEventDecode :: t -> InterchangeEventDecodeMockC t m a -> m a
mockInterchangeEventDecode t = runReader t . runInterchangeEventDecodeMockC
