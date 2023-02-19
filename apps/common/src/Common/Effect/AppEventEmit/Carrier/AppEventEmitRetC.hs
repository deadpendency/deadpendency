module Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
  ( runAppEventEmitRet,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit (AppEventEmit (..))
import Common.Effect.AppEventEmit.Model.AppEvent (AppEvent)
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype AppEventEmitRetC m a = AppEventEmitRetC {runAppEventEmitRetC :: WriterC [AppEvent] m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (AppEventEmit :+: sig) (AppEventEmitRetC m) where
  alg hdl sig ctx = AppEventEmitRetC $ case sig of
    L (EmitAppEvent appEvent) -> ctx <$ tell [appEvent]
    R other -> alg (runAppEventEmitRetC . hdl) (R other) ctx

runAppEventEmitRet :: AppEventEmitRetC m a -> m ([AppEvent], a)
runAppEventEmitRet = runWriter . runAppEventEmitRetC
