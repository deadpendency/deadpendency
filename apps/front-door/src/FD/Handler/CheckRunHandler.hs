{-# LANGUAGE DataKinds #-}

module FD.Handler.CheckRunHandler
  ( checkRunHandler,
    CheckRunRoute,
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventMessage (AppEventMessage (..))
import Common.Effect.PublishSimpleResult.Model.SimpleResult
import Common.Effect.PublishSimpleResult.PublishSimpleResult
import Common.Effect.Trace.Model.ConcludeSpanRequest
import Common.Effect.Trace.Model.StartSpanRequest
import Common.Effect.Trace.Model.StartSpanResult
import Common.Effect.Trace.TraceEmit
import Common.Handler
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Git.GitRef (GitRef (..))
import Common.Model.Git.GitSha (GitSha (..))
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.GitHub.GHAppInstallationId (GHAppInstallationId (..))
import Common.Model.GitHub.GHNodeId
import Common.Model.GitHub.GHRepoFullName
import Common.Model.GitHub.GHRepoOwnerType
import Common.Model.GitHub.GHUserName
import Common.Model.InterchangeEvent.RunCreated
import Control.Algebra (Has)
import Control.Effect.State (State, put)
import Control.Effect.Throw (Throw, throwError)
import FD.Model.AppError (AppError (..))
import GitHub.Data.Webhooks.Events (CheckRunEvent (..), CheckRunEventAction (..))
import Servant.API
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))

type CheckRunRoute =
  Header' '[Required, Strict] "X-Cloud-Trace-Context" Text
    :> GitHubEvent '[ 'WebhookCheckRunEvent]
    :> GitHubSignedReqBody '[JSON] CheckRunEvent
    :> Post '[JSON] NoContent

checkRunHandler ::
  ( Has (State (Maybe RunTrace)) sig m,
    Has (State (Maybe Run)) sig m,
    Has (Throw AppError) sig m,
    Has AppEventEmit sig m,
    Has TraceEmit sig m,
    Has (PublishSimpleResult RunCreated) sig m
  ) =>
  Text ->
  RepoWebhookEvent ->
  ((), CheckRunEvent) ->
  m NoContent
checkRunHandler googleTraceId _ (_, checkRunEvent) = do
  emitAppEventInfo (AppEventMessage "Handle check run event")

  if proceedWithRun checkRunEvent
    then do
      (StartSpanResult span) <- startSpanComponentTrace (cleanGoogleTraceId googleTraceId) (StartSpanRequest "Front Door CheckRunEvent")
      runTrace <- generateRunTrace
      put $ Just runTrace

      run <- convertPayload runTrace checkRunEvent
      put $ Just run
      let runCreated = RunCreated run
      publishSimpleResult $ SimpleResult runCreated

      concludeSpan (ConcludeSpanRequest span)
    else emitAppEventInfo (AppEventMessage "Ignoring check run event. Not rerequested action.")

  pure NoContent

convertPayload :: (Has (Throw AppError) sig m) => RunTrace -> CheckRunEvent -> m Run
convertPayload runTrace checkRunEvent = do
  let maybeInstallation = checkRunEvent ^. #evCheckRunInstallation
      maybeRepoHookUser = checkRunEvent ^? (#evCheckRunRepository . #whRepoOwner . _Right)
  case (maybeInstallation, maybeRepoHookUser) of
    (Just installation, Just repoHookUser) ->
      let checkSuite = checkRunEvent ^. (#evCheckRunCheckRun . #whCheckRunCheckSuite)
          headSha = GitSha $ checkSuite ^. #whCheckSuiteHeadSha
          maybeRef = GitRef <$> checkSuite ^. #whCheckSuiteHeadBranch
          repo = checkRunEvent ^. #evCheckRunRepository
          repoDependencyName = GHRepoFullName $ repo ^. #whRepoFullName
          repoOwner = RepoOwner $ repoHookUser ^. #whUserLogin
          repoOwnerAccountId = repoHookUser ^. #whUserId
          repoName = RepoName $ repo ^. #whRepoName
          isPrivate = repo ^. #whRepoIsPrivate
          ownerType = case checkRunEvent ^. #evCheckRunOrganization of
            Nothing -> GHROTUser
            Just _ -> GHROTOrganization
          qualifiedRepo = QualifiedRepo GitHub repoOwner repoName
          repoNodeId = GHNodeId $ repo ^. #whRepoNodeId
          triggeredUser = GHUserName $ checkRunEvent ^. (#evCheckRunSender . #whUserLogin)
          repoOwnerText = checkRunEvent ^. (#evCheckRunRepository . #whRepoOwner . choosing #whSimplUserName #whUserLogin)
          isDeadpendencyRun = repoOwnerText == "deadpendency"
          appInstallationId = GHAppInstallationId $ installation ^. #whChecksInstallationId
       in pure $
            Run
              { _runTrace = runTrace,
                _gitRef = maybeRef,
                _gitHeadSha = headSha,
                _repoDependencyName = repoDependencyName,
                _repoPrivate = isPrivate,
                _repoOwnerType = ownerType,
                _repoOwnerAccountId = repoOwnerAccountId,
                _qualifiedRepo = qualifiedRepo,
                _repoNodeId = repoNodeId,
                _triggeredUser = triggeredUser,
                _isDeadpendencyRun = isDeadpendencyRun,
                _appInstallationId = appInstallationId,
                _ghInstallationAuth = Nothing
              }
    (Nothing, _) -> throwError $ UnexpectedMissingInfoInCheckRunEvent "Installation Details"
    (_, Nothing) -> throwError $ UnexpectedMissingInfoInCheckRunEvent "Repo Owner Details"

proceedWithRun :: CheckRunEvent -> Bool
proceedWithRun checkRunEvent =
  case checkRunEvent ^. #evCheckRunAction of
    CheckRunEventActionRerequested -> True
    _ -> False
