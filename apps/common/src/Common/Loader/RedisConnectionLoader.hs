module Common.Loader.RedisConnectionLoader (loadRedisConnectionGlobal) where

import Common.Model.Cache.CacheConfigGlobal
import Control.Concurrent (newMVar)
import Database.Redis qualified as R

loadRedisConnectionGlobal :: Text -> IO CacheConfigGlobal
loadRedisConnectionGlobal redisHost = do
  putTextLn "Loading CacheConfigGlobal"
  let connectInfo =
        R.defaultConnectInfo
          { R.connectHost = unpack redisHost,
            R.connectMaxIdleTime = 55
          }
  connection <- R.checkedConnect connectInfo
  mvarConnection <- newMVar connection
  let cacheConfigGlobal =
        CacheConfigGlobal
          { _connectInfo = connectInfo,
            _mvarConnection = mvarConnection
          }
  putTextLn "CacheConfigGlobal Loaded"
  pure cacheConfigGlobal
