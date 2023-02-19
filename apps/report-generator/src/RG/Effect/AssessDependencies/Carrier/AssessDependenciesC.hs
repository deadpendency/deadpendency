module RG.Effect.AssessDependencies.Carrier.AssessDependenciesC
  ( AssessDependenciesC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.Util
import Common.Model.Error.CommonError
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.State (State)
import Control.Effect.Throw (Throw)
import Data.Vector.NonEmpty qualified as NV
import RG.Effect.AssessDependencies.AssessDependencies
import RG.Effect.AssessDependencies.Backend.AssessDependencyBackend
import RG.Effect.AssessDependencies.Model.AssessDependenciesRequest
import RG.Effect.AssessDependencies.Model.AssessDependenciesResult

newtype AssessDependenciesC m a = AssessDependenciesC {runAssessDependenciesC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (State (Maybe RepoConfig)) sig m,
    Has (Throw CommonError) sig m,
    MonadIO m
  ) =>
  Algebra (AssessDependencies :+: sig) (AssessDependenciesC m)
  where
  alg hdl sig ctx = case sig of
    (L (AssessDependencies request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Assess dependencies") (AppEventAdditional (onlyFirst25Deps request))
      currentTime <- liftIO getCurrentTime
      repoConfig <- getRepoConfig
      let rulesConfig = repoConfig ^. #_rulesConfig
          enrichedDependencies = request ^. (#_enrichedRepoDependencies . #_dependencies)
          depsDependencyAssessments = fmap (assessDependency currentTime rulesConfig) enrichedDependencies

          result =
            AssessDependenciesResult
              { _dependencyAssessments = depsDependencyAssessments
              }

      emitAppEventInfoA (AppEventMessage "Finished: Assess dependencies") (AppEventAdditional $ NV.take 25 depsDependencyAssessments)
      AssessDependenciesC $ pure (ctx $> result)
    (R other) -> AssessDependenciesC $ alg (runAssessDependenciesC . hdl) other ctx

-- too many deps will exceed the 256KB logging limit of stackdriver
onlyFirst25Deps :: AssessDependenciesRequest -> AssessDependenciesRequest
onlyFirst25Deps grr =
  grr
    & (#_enrichedRepoDependencies . #_dependencies) %~ (NV.unsafeFromVector . NV.take 25)
