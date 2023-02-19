module Common.Model.Cache.CacheConfigGlobal
  ( CacheConfigGlobal (..),
  )
where

import Control.Concurrent (MVar)
import Database.Redis qualified as R

data CacheConfigGlobal = CacheConfigGlobal
  { _connectInfo :: R.ConnectInfo,
    _mvarConnection :: MVar R.Connection
  }
  deriving stock (Generic)
