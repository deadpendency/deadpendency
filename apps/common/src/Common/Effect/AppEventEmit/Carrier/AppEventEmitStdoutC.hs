module Common.Effect.AppEventEmit.Carrier.AppEventEmitStdoutC
  ( AppEventEmitStdoutIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit (AppEventEmit (..))
import Control.Algebra (Algebra (..), (:+:) (..))

newtype AppEventEmitStdoutIOC m a = AppEventEmitStdoutIOC {runAppEventEmitStdoutIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (AppEventEmit :+: sig) (AppEventEmitStdoutIOC m) where
  alg hdl sig ctx = case sig of
    L (EmitAppEvent appEvent) -> ctx <$ liftIO (putTextLn $ appEvent ^. #_message . #_ntText)
    R other -> AppEventEmitStdoutIOC (alg (runAppEventEmitStdoutIOC . hdl) other ctx)
