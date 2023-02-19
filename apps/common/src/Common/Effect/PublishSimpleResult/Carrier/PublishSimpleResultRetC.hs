module Common.Effect.PublishSimpleResult.Carrier.PublishSimpleResultRetC
  ( runPublishSimpleResultRet,
  )
where

import Common.Effect.PublishSimpleResult.Model.SimpleResult
import Common.Effect.PublishSimpleResult.PublishSimpleResult (PublishSimpleResult (..))
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype PublishSimpleResultRetC (p :: Type) m a = PublishSimpleResultRetC {runPublishSimpleResultRetC :: WriterC [SimpleResult p] m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (PublishSimpleResult p :+: sig) (PublishSimpleResultRetC p m) where
  alg hdl sig ctx = case sig of
    (L (PublishSimpleResult jsonPayload)) -> PublishSimpleResultRetC $ ctx <$ tell [jsonPayload]
    (R other) -> PublishSimpleResultRetC $ alg (runPublishSimpleResultRetC . hdl) (R other) ctx

runPublishSimpleResultRet :: PublishSimpleResultRetC p m a -> m ([SimpleResult p], a)
runPublishSimpleResultRet = runWriter . runPublishSimpleResultRetC
