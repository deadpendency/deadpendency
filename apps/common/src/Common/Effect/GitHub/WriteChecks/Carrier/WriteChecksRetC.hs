module Common.Effect.GitHub.WriteChecks.Carrier.WriteChecksRetC
  ( runWriteChecksRet,
  )
where

import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import Common.Effect.GitHub.WriteChecks.WriteChecks (WriteChecks (..))
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)

newtype WriteChecksRetC m a = WriteChecksRetC {runWriteChecksRetC :: ReaderC CheckRunCreateResult (WriterC [CheckRunCreateRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (WriteChecks :+: sig) (WriteChecksRetC m) where
  alg hdl sig ctx = case sig of
    (L (CreateCheckRun request)) -> WriteChecksRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> WriteChecksRetC $ alg (runWriteChecksRetC . hdl) (R (R other)) ctx
    _ -> error "this is only for testing create check run for now"

runWriteChecksRet :: CheckRunCreateResult -> WriteChecksRetC m a -> m ([CheckRunCreateRequest], a)
runWriteChecksRet checkRunCreateResult = runWriter . runReader checkRunCreateResult . runWriteChecksRetC
