module RF.Effect.FinishRun.Carrier.FinishRunRetC
  ( runFinishRunRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)
import RF.Effect.FinishRun.FinishRun
import RF.Effect.FinishRun.Model.FinishRunRequest
import RF.Effect.FinishRun.Model.FinishRunResult

newtype FinishRunRetC m a = FinishRunRetC {runFinishRunRetC :: ReaderC FinishRunResult (WriterC [FinishRunRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (FinishRun :+: sig) (FinishRunRetC m) where
  alg hdl sig ctx = case sig of
    (L (FinishRun request)) -> FinishRunRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> FinishRunRetC $ alg (runFinishRunRetC . hdl) (R (R other)) ctx

runFinishRunRet :: FinishRunResult -> FinishRunRetC m a -> m ([FinishRunRequest], a)
runFinishRunRet result = runWriter . runReader result . runFinishRunRetC
