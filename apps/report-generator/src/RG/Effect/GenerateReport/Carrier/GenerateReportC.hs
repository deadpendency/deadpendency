module RG.Effect.GenerateReport.Carrier.GenerateReportC
  ( GenerateReportC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Model.Config.CommonConfig
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Reader (Reader, ask)
import Control.Effect.Throw (Throw)
import Data.Vector.NonEmpty qualified as NV
import RG.Effect.GenerateReport.Backend.GenerateOverallReportBackend
import RG.Effect.GenerateReport.GenerateReport
import RG.Effect.GenerateReport.Model.GenerateReportRequest
import RG.Effect.GenerateReport.Model.GenerateReportResult

newtype GenerateReportC m a = GenerateReportC {runGenerateReportC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Reader CommonConfig) sig m,
    Has (Throw CommonError) sig m,
    MonadIO m
  ) =>
  Algebra (GenerateReport :+: sig) (GenerateReportC m)
  where
  alg hdl sig ctx = case sig of
    (L (GenerateReport request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Generate report") (AppEventAdditional (onlyFirst25Deps request))
      commonConfig <- ask @CommonConfig
      let appVersion = commonConfig ^. #_appVersion
          ignoredDependencies = request ^. (#_ignoredRepoDependencies . #_dependencies)
          erroredDependencies = request ^. (#_erroredRepoDependencies . #_dependencies)
          dependencyAssessments = request ^. #_dependencyAssessments
          overallReport = generateOverallReport appVersion erroredDependencies ignoredDependencies dependencyAssessments

          result =
            GenerateReportResult
              { _dependencyReport = overallReport
              }

      emitAppEventInfo (AppEventMessage "Finished: Generate report")
      GenerateReportC $ pure (ctx $> result)
    (R other) -> GenerateReportC $ alg (runGenerateReportC . hdl) other ctx

-- too many deps will exceed the 256KB logging limit of stackdriver
onlyFirst25Deps :: GenerateReportRequest -> GenerateReportRequest
onlyFirst25Deps grr =
  grr
    & #_dependencyAssessments %~ (NV.unsafeFromVector . NV.take 25)
