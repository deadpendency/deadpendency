module Common.Effect.CacheExternal.Carrier.CacheExternalRetC
  ( runCacheExternalRet,
  )
where

import Common.Effect.CacheExternal.CacheExternal (CacheExternal (..))
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype CacheExternalRetC (p :: Type) m a = CacheExternalRetC {runCacheExternalRetC :: ReaderC (Maybe p) (WriterC [(Integer, Text)] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (CacheExternal p :+: sig) (CacheExternalRetC p m) where
  alg hdl sig ctx = case sig of
    (L (LoadFromCache _)) -> error "Not implemented"
    (L (LoadFromCacheSingle _)) -> CacheExternalRetC $ ask @(Maybe p) <&> (<$ ctx)
    (L (StoreInCache _ _)) -> error "Not implemented"
    (L (StoreInCacheSingle expire key _)) -> CacheExternalRetC $ tell [(expire, key)] <&> (ctx $>)
    (R other) -> CacheExternalRetC $ alg (runCacheExternalRetC . hdl) (R (R other)) ctx

runCacheExternalRet :: Maybe p -> CacheExternalRetC p m a -> m ([(Integer, Text)], a)
runCacheExternalRet t = runWriter . runReader t . runCacheExternalRetC
