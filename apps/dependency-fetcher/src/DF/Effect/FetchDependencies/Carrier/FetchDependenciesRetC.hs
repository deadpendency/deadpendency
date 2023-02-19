module DF.Effect.FetchDependencies.Carrier.FetchDependenciesRetC
  ( runFetchDependenciesRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)
import DF.Effect.FetchDependencies.FetchDependencies
import DF.Effect.FetchDependencies.Model.FetchDependenciesRequest
import DF.Effect.FetchDependencies.Model.FetchDependenciesResult

newtype FetchDependenciesRetC m a = FetchDependenciesRetC {runFetchDependenciesRetC :: ReaderC FetchDependenciesResult (WriterC [FetchDependenciesRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (FetchDependencies :+: sig) (FetchDependenciesRetC m) where
  alg hdl sig ctx = case sig of
    (L (FetchDependencies request)) -> FetchDependenciesRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> FetchDependenciesRetC $ alg (runFetchDependenciesRetC . hdl) (R (R other)) ctx

runFetchDependenciesRet :: FetchDependenciesResult -> FetchDependenciesRetC m a -> m ([FetchDependenciesRequest], a)
runFetchDependenciesRet result = runWriter . runReader result . runFetchDependenciesRetC
