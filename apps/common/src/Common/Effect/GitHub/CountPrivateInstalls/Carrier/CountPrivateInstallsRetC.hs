module Common.Effect.GitHub.CountPrivateInstalls.Carrier.CountPrivateInstallsRetC
  ( runCountPrivateInstallsRet,
  )
where

import Common.Effect.GitHub.CountPrivateInstalls.CountPrivateInstalls (CountPrivateInstalls (..))
import Common.Effect.GitHub.CountPrivateInstalls.Model.CountPrivateInstallsResult
import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.Reader (ask)

newtype CountPrivateInstallsRetC m a = CountPrivateInstallsRetC {runCountPrivateInstallsRetC :: ReaderC CountPrivateInstallsResult m a}
  deriving newtype (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (CountPrivateInstalls :+: sig) (CountPrivateInstallsRetC m) where
  alg hdl sig ctx = case sig of
    (L PrivateInstallsCount) -> CountPrivateInstallsRetC $ ask @CountPrivateInstallsResult <&> (<$ ctx)
    (R other) -> CountPrivateInstallsRetC $ alg (runCountPrivateInstallsRetC . hdl) (R other) ctx

runCountPrivateInstallsRet :: CountPrivateInstallsResult -> CountPrivateInstallsRetC m a -> m a
runCountPrivateInstallsRet t = runReader t . runCountPrivateInstallsRetC
