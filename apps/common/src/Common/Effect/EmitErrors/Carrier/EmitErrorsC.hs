module Common.Effect.EmitErrors.Carrier.EmitErrorsC (EmitErrorsC (..)) where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEvent
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventLevel
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Model.Error.ConsideredAppFailure
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Error.Either
import Data.Aeson

newtype EmitErrorsC (e :: Type) m a = EmitErrorsC {runEmitErrorsC :: ErrorC e m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Has AppEventEmit sig m, ToJSON e, ConsideredAppFailure e) => Algebra (Error e :+: sig) (EmitErrorsC e m) where
  alg hdl sig ctx = case sig of
    (L (L (Throw e))) -> do
      let (appEventLevel, message) =
            if consideredAppFailure e
              then (AppEventLevelError, "Request Error")
              else (AppEventLevelWarning, "Request Warning")

      emitAppEvent $ AppEvent appEventLevel (AppEventMessage message) (Just $ AppEventAdditional e)
      -- defer to ErrorC for the actual functionality
      EmitErrorsC $ alg (runEmitErrorsC . hdl) sig ctx
    _ -> EmitErrorsC $ alg (runEmitErrorsC . hdl) sig ctx
