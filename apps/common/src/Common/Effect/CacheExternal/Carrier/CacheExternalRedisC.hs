module Common.Effect.CacheExternal.Carrier.CacheExternalRedisC (CacheExternalRedisIOC (..)) where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.CacheExternal.CacheExternal
import Common.Model.Cache.CacheConfigGlobal
import Common.Model.Config.AppEnv
import Common.Model.Config.AppVersion
import Common.Model.Config.CommonConfig
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Error.Either
import Control.Concurrent (putMVar, readMVar, takeMVar)
import Control.Effect.Reader (Reader, ask)
import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Database.Redis qualified as R

newtype CacheExternalRedisIOC (t :: Type) m a = CacheExternalRedisIOC {runCacheExternalRedisIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has (Reader CacheConfigGlobal) sig m,
    Has (Reader CommonConfig) sig m,
    Has (Throw CommonError) sig m,
    Has AppEventEmit sig m,
    MonadIO m
  ) =>
  Algebra (CacheExternal t :+: sig) (CacheExternalRedisIOC t m)
  where
  alg hdl sig ctx = case sig of
    (L (LoadFromCache keys)) -> do
      resultMap <- loadFromCacheInternal keys
      CacheExternalRedisIOC $ pure (ctx $> resultMap)
    (L (LoadFromCacheSingle key)) -> do
      resultMap <- loadFromCacheInternal $ NV.singleton key
      let maybeResult = HM.lookup key resultMap
      CacheExternalRedisIOC $ pure (ctx $> maybeResult)
    (L (StoreInCache expireInSeconds hashMap)) -> do
      storeInCacheInternal expireInSeconds hashMap
      CacheExternalRedisIOC $ pure ctx
    (L (StoreInCacheSingle expireInSeconds key value)) -> do
      storeInCacheInternal expireInSeconds (HM.singleton key value)
      CacheExternalRedisIOC $ pure ctx
    (R other) -> CacheExternalRedisIOC $ alg (runCacheExternalRedisIOC . hdl) other ctx

loadFromCacheInternal ::
  ( FromJSON b,
    MonadIO m,
    Has (Throw CommonError) sig m,
    Has (Reader CacheConfigGlobal) sig m,
    Has (Reader CommonConfig) sig m,
    Has AppEventEmit sig m
  ) =>
  NV.NonEmptyVector Text ->
  m (HashMap Text b)
loadFromCacheInternal keysToLoad = do
  emitAppEventInfoA (AppEventMessage "Started: Load From Cache") (AppEventAdditional keysToLoad)
  commonConfig <- ask @CommonConfig
  let appVersion = commonConfig ^. #_appVersion
      appEnv = commonConfig ^. #_appEnv
      appendedVersion = fmap (buildKey appEnv appVersion) keysToLoad
      avAsList = NV.toList appendedVersion
  eitherReplyResults <- callRedisWithRetryConn (R.mget avAsList)
  listMaybeResultA <- liftEither $ first (CacheError . ("Failure to load from cache: " <>) . show) eitherReplyResults
  nvMaybeResultA <- liftEither $ maybeToRight (CacheError "Unexpected empty results list") $ NV.fromList listMaybeResultA
  let keyMatches = NV.zip keysToLoad nvMaybeResultA
      filterNothings = NV.mapMaybe (sequenceAOf _2) keyMatches
  decoded <- liftEither $ traverse decodeKeyValue filterNothings
  let asMap = HM.fromList $ V.toList decoded
  emitAppEventInfoA (AppEventMessage "Finished: Load From Cache") (AppEventAdditional (fmap fst decoded))
  pure asMap

storeInCacheInternal ::
  ( ToJSON a,
    MonadIO m,
    Has (Throw CommonError) sig m,
    Has (Reader CacheConfigGlobal) sig m,
    Has (Reader CommonConfig) sig m,
    Has AppEventEmit sig m
  ) =>
  Integer ->
  HashMap Text a ->
  m ()
storeInCacheInternal expireInSeconds toSaveHashMap = do
  emitAppEventInfoA (AppEventMessage "Started: Store In Cache") (AppEventAdditional (HM.keys toSaveHashMap))

  when
    (HM.null toSaveHashMap)
    (throwError (CacheError "Unexpected called with empty map"))

  commonConfig <- ask @CommonConfig

  let appendedKeysList = HM.toList toSaveHashMap
      appVersion = commonConfig ^. #_appVersion
      appEnv = commonConfig ^. #_appEnv
      updatedKeyValues = fmap (\(key, value) -> (buildKey appEnv appVersion key, toStrict $ encode value)) appendedKeysList

      -- it is possible the transaction increases memory use? Should test this. Minimise memory is more valuable than performance
      redisOperation :: R.Redis (R.TxResult ())
      redisOperation = R.multiExec $ do
        R.mset updatedKeyValues
        traverse_ (`R.expire` expireInSeconds) (fmap fst updatedKeyValues)
        pure $ pure ()

  result <- callRedisWithRetryConn redisOperation

  case result of
    R.TxSuccess _ -> pure ()
    other ->
      throwError $ CacheError $ "Unexpected failure to store in cache: " <> show other

  emitAppEventInfo (AppEventMessage "Finished: Store In Cache")

buildKey :: AppEnv -> AppVersion -> Text -> ByteString
buildKey appEnv appVersion keyText =
  let appEnvText = appEnvAsText appEnv
      appVersionText = appVersion ^. #_ntText
   in encodeUtf8 $ appEnvText <> "-" <> appVersionText <> "-" <> keyText

decodeKeyValue :: (FromJSON a) => (Text, ByteString) -> Either CommonError (Text, a)
decodeKeyValue (key, rawValue) =
  bimap (CacheError . ("Unable to parse json from cache: " <>) . pack) (key,) (eitherDecodeStrict' rawValue)

callRedisWithRetryConn ::
  (Has (Reader CacheConfigGlobal) sig m, MonadIO m) =>
  forall b.
  R.Redis b ->
  m b
callRedisWithRetryConn a = do
  conn <- getConnection
  eitherConnError <- liftIO $ try @_ @R.ConnectionLostException $ R.runRedis conn a
  case eitherConnError of
    Right result -> pure result
    Left _ -> do
      newConn <- newConnectionUpdateMvar
      liftIO $ R.runRedis newConn a

getConnection :: (Has (Reader CacheConfigGlobal) sig m, MonadIO m) => m R.Connection
getConnection = do
  cacheConfig <- ask @CacheConfigGlobal
  liftIO $ readMVar $ cacheConfig ^. #_mvarConnection

newConnectionUpdateMvar :: (Has (Reader CacheConfigGlobal) sig m, MonadIO m) => m R.Connection
newConnectionUpdateMvar = do
  cacheConfig <- ask @CacheConfigGlobal
  let connectInfo = cacheConfig ^. #_connectInfo
      mvarConnection = cacheConfig ^. #_mvarConnection
  liftIO $ do
    takeMVar mvarConnection
    conn <- R.checkedConnect connectInfo
    putMVar mvarConnection conn
    pure conn
