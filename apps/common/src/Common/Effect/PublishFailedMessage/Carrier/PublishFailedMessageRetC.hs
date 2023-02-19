module Common.Effect.PublishFailedMessage.Carrier.PublishFailedMessageRetC
  ( runPublishFailedMessageRet,
  )
where

import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Common.Effect.PublishFailedMessage.PublishFailedMessage (PublishFailedMessage (..))
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype PublishFailedMessageRetC p (m :: (Type -> Type)) a = PublishFailedMessageRetC {runPublishFailedMessageRetC :: WriterC [FailedInterchangeEvent p] m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (PublishFailedMessage p :+: sig) (PublishFailedMessageRetC p m) where
  alg hdl sig ctx = case sig of
    (L (PublishFailedMessage failedInterchangeEvent)) -> PublishFailedMessageRetC $ ctx <$ tell [failedInterchangeEvent]
    (R other) -> PublishFailedMessageRetC $ alg (runPublishFailedMessageRetC . hdl) (R other) ctx

runPublishFailedMessageRet :: PublishFailedMessageRetC p m a -> m ([FailedInterchangeEvent p], a)
runPublishFailedMessageRet = runWriter . runPublishFailedMessageRetC
