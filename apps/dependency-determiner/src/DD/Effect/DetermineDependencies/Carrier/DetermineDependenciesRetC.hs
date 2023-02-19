module DD.Effect.DetermineDependencies.Carrier.DetermineDependenciesRetC
  ( DetermineDependenciesRetC (..),
    runDetermineDependenciesRet,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Writer.Strict (WriterC, runWriter)
import Control.Effect.Writer (tell)
import DD.Effect.DetermineDependencies.DetermineDependencies (DetermineDependencies (..))
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesRequest
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesResult

newtype DetermineDependenciesRetC m a = DetermineDependenciesRetC {runDetermineDependenciesRetC :: ReaderC DetermineDependenciesResult (WriterC [DetermineDependenciesRequest] m) a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (DetermineDependencies :+: sig) (DetermineDependenciesRetC m) where
  alg hdl sig ctx = case sig of
    (L (DetermineDependencies request)) -> DetermineDependenciesRetC $ tell [request] *> ask <&> (ctx $>)
    (R other) -> DetermineDependenciesRetC $ alg (runDetermineDependenciesRetC . hdl) (R (R other)) ctx

runDetermineDependenciesRet :: DetermineDependenciesResult -> DetermineDependenciesRetC m a -> m ([DetermineDependenciesRequest], a)
runDetermineDependenciesRet determineDependenciesResult = runWriter . runReader determineDependenciesResult . runDetermineDependenciesRetC
