module Handler.CheckRunHandlerSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.PublishSimpleResult.Carrier.PublishSimpleResultRetC
import Common.Effect.PublishSimpleResult.Model.SimpleResult
import Common.Effect.Trace.Carrier.TraceEmitRetC
import Common.Effect.Trace.Model.Span
import Common.Effect.Trace.Model.StartSpanResult
import Common.Model.Details.Run
import Common.Model.Details.RunTrace
import Common.Model.Git.GitRef
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.GitHub.GHAppInstallationId
import Common.Model.GitHub.GHNodeId
import Common.Model.GitHub.GHRepoFullName
import Common.Model.GitHub.GHRepoOwnerType
import Common.Model.GitHub.GHUserName
import Common.Model.InterchangeEvent.RunCreated
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Throw.Either (runThrow)
import FD.Handler.CheckRunHandler (checkRunHandler)
import FD.Model.AppError (AppError (..))
import Gen.ChecksEventGen
import Gen.CommonEventGen
import GitHub.Data.Webhooks.Events (CheckRunEventAction (..))
import Hedgehog.Gen (sample)
import Servant.GitHub.Webhook
import Test.Hspec
import Test.Hspec.Hedgehog
import Text.Read (read)

spec :: Spec
spec = parallel $
  context "When processing events" $ do
    let runTrace = RunTrace "run-trace"
        startSpanResult = StartSpanResult $ Span "SpanId" "SpanName" (read "2020-06-01 00:00:00 UTC")
        runHandler =
          runThrow @AppError
            >>> runState @(Maybe Run) Nothing
            >>> runState @(Maybe RunTrace) Nothing
            >>> runTraceEmitRet runTrace startSpanResult
            >>> runAppEventEmitRet
            >>> runPublishSimpleResultRet @RunCreated
            >>> runIdentity

    it "a happy day test produces correct results" $
      hedgehog $ do
        event <- forAll genCheckRunEvent
        let massagedEvent =
              event
                & #evCheckRunAction .~ CheckRunEventActionRerequested
                & (#evCheckRunInstallation . _Just . #whChecksInstallationId) .~ 12345678
                & (#evCheckRunCheckRun . #whCheckRunCheckSuite . #whCheckSuiteHeadBranch) ?~ "git-ref"
                & (#evCheckRunCheckRun . #whCheckRunCheckSuite . #whCheckSuiteHeadSha) .~ "git-sha"
                & (#evCheckRunRepository . #whRepoFullName) .~ "git-repo-dependencyname"
                & (#evCheckRunRepository . #whRepoName) .~ "git-repo-name"
                & (#evCheckRunRepository . #whRepoIsPrivate) .~ True
                & (#evCheckRunRepository . #whRepoOwner . _Right . #whUserLogin) .~ "git-repo-owner"
                & (#evCheckRunRepository . #whRepoOwner . _Right . #whUserId) .~ 1234
                & (#evCheckRunRepository . #whRepoNodeId) .~ "git-repo-node-id"
                & (#evCheckRunSender . #whUserLogin) .~ "git-user-name"
                & #evCheckRunOrganization .~ Nothing

        let handlerResult = checkRunHandler "asdf/asdf" WebhookCheckRunEvent ((), massagedEvent)
            (simpleResults, (appEvents, (concludeSpanRequests, (startSpanRequests, (putRunTrace, (putRun, appErrors)))))) = runHandler handlerResult

        annotate "did not throw any errors"
        isRight appErrors === True

        let expectedRun =
              Run
                { _runTrace = runTrace,
                  _gitRef = Just (GitRef "git-ref"),
                  _gitHeadSha = GitSha "git-sha",
                  _repoDependencyName = GHRepoFullName "git-repo-dependencyname",
                  _repoPrivate = True,
                  _repoOwnerType = GHROTUser,
                  _repoOwnerAccountId = 1234,
                  _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "git-repo-owner") (RepoName "git-repo-name"),
                  _repoNodeId = GHNodeId "git-repo-node-id",
                  _triggeredUser = GHUserName "git-user-name",
                  _isDeadpendencyRun = False,
                  _appInstallationId = GHAppInstallationId 12345678,
                  _ghInstallationAuth = Nothing
                }
            expectedRunCreated = RunCreated expectedRun

        annotate "produces the correct run created event"
        simpleResults === [SimpleResult expectedRunCreated]

        annotate "and put it into state"
        putRun === Just expectedRun

        annotate "emitted an app event"
        genericLength @Integer appEvents === 1

        let expectedRunTrace = Just runTrace

        annotate "the correct run trace state was put"
        putRunTrace === expectedRunTrace

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

    it "missing installation id produces expected error" $ do
      event <- sample genCheckRunEvent
      let massagedEvent =
            event
              & #evCheckRunAction .~ CheckRunEventActionRerequested
              & #evCheckRunInstallation .~ Nothing
          handlerResult = checkRunHandler "asdf/asdf" WebhookCheckRunEvent ((), massagedEvent)
          (runsCreated, (_, (_, (_, (_, (_, appErrors)))))) = runHandler handlerResult

      appErrors `shouldBe` Left (UnexpectedMissingInfoInCheckRunEvent "Installation Details")
      runsCreated `shouldBe` []

    it "missing complex user produces expected error" $
      hedgehog $ do
        event <- sample genCheckRunEvent
        simpleUser <- forAll genHookSimpleUser
        let massagedEvent =
              event
                & #evCheckRunAction .~ CheckRunEventActionRerequested
                & (#evCheckRunRepository . #whRepoOwner) .~ Left simpleUser
            handlerResult = checkRunHandler "asdf/asdf" WebhookCheckRunEvent ((), massagedEvent)
            (runsCreated, (_, (_, (_, (_, (_, appErrors)))))) = runHandler handlerResult

        appErrors === Left (UnexpectedMissingInfoInCheckRunEvent "Repo Owner Details")
        runsCreated === []

    it "non-rerequested actions do not initialize a run" $
      hedgehog $ do
        event <- sample genCheckRunEvent
        let massagedEvent =
              event
                & #evCheckRunAction .~ CheckRunEventActionCompleted
            handlerResult = checkRunHandler "asdf/asdf" WebhookCheckRunEvent ((), massagedEvent)
            (runsCreated, (_, (_, (_, (_, (_, appErrors)))))) = runHandler handlerResult

        annotate "did not throw any errors"
        isRight appErrors === True

        annotate "did not create any runs"
        runsCreated === []
