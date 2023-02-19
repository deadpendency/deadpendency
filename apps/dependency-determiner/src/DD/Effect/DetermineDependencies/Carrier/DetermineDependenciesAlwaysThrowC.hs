module DD.Effect.DetermineDependencies.Carrier.DetermineDependenciesAlwaysThrowC
  ( DetermineDependenciesAlwaysThrowC (..),
    runDetermineDependenciesAlwaysThrow,
  )
where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Throw.Either
import DD.Effect.DetermineDependencies.DetermineDependencies (DetermineDependencies (..))
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError

newtype DetermineDependenciesAlwaysThrowC m a = DetermineDependenciesAlwaysThrowC {runDetermineDependenciesAlwaysThrowC :: ReaderC DetermineDependenciesError m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m, Has (Throw DetermineDependenciesError) sig m) => Algebra (DetermineDependencies :+: sig) (DetermineDependenciesAlwaysThrowC m) where
  alg hdl sig ctx = case sig of
    (L (DetermineDependencies _)) ->
      DetermineDependenciesAlwaysThrowC $ ask @DetermineDependenciesError >>= throwError
    (R other) -> DetermineDependenciesAlwaysThrowC $ alg (runDetermineDependenciesAlwaysThrowC . hdl) (R other) ctx

runDetermineDependenciesAlwaysThrow ::
  DetermineDependenciesError ->
  DetermineDependenciesAlwaysThrowC m a ->
  m a
runDetermineDependenciesAlwaysThrow theError = runReader theError . runDetermineDependenciesAlwaysThrowC
