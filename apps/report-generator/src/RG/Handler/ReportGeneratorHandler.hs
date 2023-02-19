{-# LANGUAGE DataKinds #-}

module RG.Handler.ReportGeneratorHandler
  ( reportGeneratorHandler,
    ReportGeneratorRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.InterchangeEventLoad.InterchangeEventLoad (InterchangeEventLoad (..), loadInterchangeEvent)
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishComponentResult.PublishComponentResult
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.InterchangeEvent.DependenciesFetched
import Common.Model.InterchangeEvent.RunResult
import Control.Algebra (Has)
import Network.Google.PubSub qualified as G
import RG.Effect.AssessDependencies.AssessDependencies
import RG.Effect.AssessDependencies.Model.AssessDependenciesRequest
import RG.Effect.AssessDependencies.Model.AssessDependenciesResult
import RG.Effect.GenerateReport.GenerateReport
import RG.Effect.GenerateReport.Model.GenerateReportRequest
import RG.Effect.GenerateReport.Model.GenerateReportResult
import Servant.API

type ReportGeneratorRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> ReqBody '[JSON] G.ReceivedMessage
    :> Post '[JSON] NoContent

reportGeneratorHandler ::
  ( Has AppEventEmit sig m,
    Has (PublishComponentResult RunResult) sig m,
    Has (InterchangeEventLoad DependenciesFetched) sig m,
    Has TraceEmit sig m,
    Has AssessDependencies sig m,
    Has GenerateReport sig m
  ) =>
  Text ->
  G.ReceivedMessage ->
  m NoContent
reportGeneratorHandler googleTraceId receivedMessage = do
  (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Report Generator")

  -- decode queue event and log and put in state
  interchangeEvent <- loadInterchangeEvent receivedMessage
  let dependenciesFetched = interchangeEvent ^. #_result

  emitAppEventInfo (AppEventMessage "Receive report generator")

  -- assess dependencies
  let assessDependenciesRequest = createAssessDependenciesRequest dependenciesFetched
  assessDependenciesResult <- assessDependencies assessDependenciesRequest

  -- produce the report
  let generateReportRequest = createGenerateReportRequest dependenciesFetched assessDependenciesResult
  generateReportResult <- generateReport generateReportRequest

  -- publish result to the queue
  let reportGenerated = createReportGenerated generateReportResult
  publishComponentResult $ SuccessComponentResult reportGenerated

  -- finish up
  concludeSpan (ConcludeSpanRequest span)
  pure NoContent

createAssessDependenciesRequest :: DependenciesFetched -> AssessDependenciesRequest
createAssessDependenciesRequest dependenciesFetched =
  let enrichedRepoDependencies = dependenciesFetched ^. #_enrichedRepoDependencies
   in AssessDependenciesRequest
        { _enrichedRepoDependencies = enrichedRepoDependencies
        }

createGenerateReportRequest :: DependenciesFetched -> AssessDependenciesResult -> GenerateReportRequest
createGenerateReportRequest dependenciesFetched assessDependenciesResult =
  let erroredRepoDependencies = dependenciesFetched ^. #_erroredRepoDependencies
      ignoredRepoDependencies = dependenciesFetched ^. #_ignoredRepoDependencies
      dependencyAssessments = assessDependenciesResult ^. #_dependencyAssessments
   in GenerateReportRequest
        { _dependencyAssessments = dependencyAssessments,
          _erroredRepoDependencies = erroredRepoDependencies,
          _ignoredRepoDependencies = ignoredRepoDependencies
        }

createReportGenerated :: GenerateReportResult -> RunResult
createReportGenerated generateReportResult =
  let overallReport = generateReportResult ^. #_dependencyReport
   in RunSuccess overallReport
