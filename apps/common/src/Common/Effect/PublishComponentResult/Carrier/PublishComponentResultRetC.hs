module Common.Effect.PublishComponentResult.Carrier.PublishComponentResultRetC
  ( runPublishComponentResultRet,
  )
where

import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult (PublishComponentResult (..))
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype PublishComponentResultRetC (p :: Type) m a = PublishComponentResultRetC {runPublishComponentResultRetC :: WriterC [ComponentResult p] m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (PublishComponentResult p :+: sig) (PublishComponentResultRetC p m) where
  alg hdl sig ctx = case sig of
    (L (PublishComponentResult jsonPayload)) -> PublishComponentResultRetC $ ctx <$ tell [jsonPayload]
    (R other) -> PublishComponentResultRetC $ alg (runPublishComponentResultRetC . hdl) (R other) ctx

runPublishComponentResultRet :: PublishComponentResultRetC p m a -> m ([ComponentResult p], a)
runPublishComponentResultRet = runWriter . runPublishComponentResultRetC
