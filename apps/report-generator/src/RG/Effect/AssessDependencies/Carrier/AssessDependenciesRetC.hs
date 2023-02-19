module RG.Effect.AssessDependencies.Carrier.AssessDependenciesRetC
  ( runAssessDependenciesRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)
import RG.Effect.AssessDependencies.AssessDependencies
import RG.Effect.AssessDependencies.Model.AssessDependenciesRequest
import RG.Effect.AssessDependencies.Model.AssessDependenciesResult

newtype AssessDependenciesRetC m a = AssessDependenciesRetC {runAssessDependenciesRetC :: ReaderC AssessDependenciesResult (WriterC [AssessDependenciesRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (AssessDependencies :+: sig) (AssessDependenciesRetC m) where
  alg hdl sig ctx = case sig of
    (L (AssessDependencies request)) -> AssessDependenciesRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> AssessDependenciesRetC $ alg (runAssessDependenciesRetC . hdl) (R (R other)) ctx

runAssessDependenciesRet :: AssessDependenciesResult -> AssessDependenciesRetC m a -> m ([AssessDependenciesRequest], a)
runAssessDependenciesRet result = runWriter . runReader result . runAssessDependenciesRetC
