module Common.Effect.InterchangeEventDecode.Carrier.InterchangeEventDecodeC
  ( InterchangeEventDecodeC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.InterchangeEventDecode.InterchangeEventDecode (InterchangeEventDecode (..))
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither, throwError)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict')
import Data.ByteString.Base64 qualified as B64
import Network.Google.PubSub qualified as G

newtype InterchangeEventDecodeC (t :: Type) m a = InterchangeEventDecodeC {runInterchangeEventDecodeC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Throw CommonError) sig m,
    FromJSON t,
    ToJSON t
  ) =>
  Algebra (InterchangeEventDecode t :+: sig) (InterchangeEventDecodeC t m)
  where
  alg hdl sig ctx = case sig of
    (L (DecodeInterchangeEvent rm)) -> do
      emitAppEventInfo (AppEventMessage "Started: Interchange Event Decode")
      let (maybeData :: Maybe ByteString) = rm ^? (G.rmMessage . _Just . G.pmData . _Just)
      rawMessage <- case maybeData of
        Just data' -> decodeToMessage data'
        Nothing -> throwError PubSubMessageMissing
      event <- messageToInterchangeEvent rawMessage
      emitAppEventInfo (AppEventMessage "Finished: Interchange Event Decode")
      InterchangeEventDecodeC $ pure (ctx $> event)
    (R other) -> InterchangeEventDecodeC $ alg (runInterchangeEventDecodeC . hdl) other ctx

decodeToMessage :: (Has (Throw CommonError) sig m) => ByteString -> m ByteString
decodeToMessage inputBs = (liftEither . first (PubSubMessageBytestringDecodeFailure (decodeUtf8 inputBs)) . B64.decodeBase64) inputBs

messageToInterchangeEvent :: (Has (Throw CommonError) sig m, FromJSON t) => ByteString -> m t
messageToInterchangeEvent = liftEither . first (PubSubMessageToJSONDecodeFailure . pack) . eitherDecodeStrict'
