module Handler.DependencyDeterminerHandlerSpec (spec) where

import Common.Effect.AppEventEmit.Carrier.AppEventEmitRetC
import Common.Effect.InterchangeEventLoad.Carrier.InterchangeEventLoadMockC
import Common.Effect.PublishComponentResult.Carrier.PublishComponentResultRetC
import Common.Effect.PublishComponentResult.Model.ComponentResult
import Common.Effect.PublishFailedMessage.Carrier.PublishFailedMessageRetC
import Common.Effect.PublishFailedMessage.Model.FailedInterchangeEvent
import Common.Effect.Trace.Carrier.TraceEmitRetC
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Error.ProcessingError
import Common.Model.Error.UserError
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.InterchangeEvent.DependenciesDetermined
import Common.Model.InterchangeEvent.InterchangeEvent
import Common.Model.InterchangeEvent.RunPrepared
import CommonTest.Gen.Effect.TraceEmit
import CommonTest.Gen.Gogol.PubSub
import CommonTest.Gen.Model.Details
import CommonTest.Gen.Model.GitHub
import CommonTest.Gen.Model.GitHub.Checks.CheckRun
import CommonTest.Gen.Model.RepoConfig
import Control.Carrier.Error.Either (runError)
import DD.Effect.DetermineDependencies.Carrier.DetermineDependenciesAlwaysThrowC
import DD.Effect.DetermineDependencies.Carrier.DetermineDependenciesRetC
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesRequest
import DD.Handler.DependencyDeterminerHandler (dependencyDeterminerHandler)
import Data.Vector qualified as V
import Gen.Model.DetermineDependencies.API
import Hedgehog.Gen (sample)
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = parallel $
  context "When processing events" $ do
    it "a happy day test produces correct results" $
      hedgehog $ do
        run <- forAll genRun
        determineDependenciesResult <- forAll genDetermineDependenciesResult
        receivedMessage <- sample genPubSubReceivedMessage
        ghInstallationAuth <- sample genGHInstallationAuth
        repoConfig <- sample genRepoConfig
        checkRun <- sample genCheckRun
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let massagedRun =
              run
                & (#_gitHeadSha . #_ntText) .~ "head-sha"
                & (#_qualifiedRepo . #_repoOwner . #_ntText) .~ "repo-owner"
                & (#_qualifiedRepo . #_repoName . #_ntText) .~ "repo-name"
                & (#_qualifiedRepo . #_repoHost) .~ GitHub
            interchangeEvent = InterchangeEvent ghInstallationAuth checkRun massagedRun repoConfig False (RunPrepared $ V.singleton Python)

        let runHandler =
              runError @DetermineDependenciesError
                >>> runTraceEmitRet runTrace startSpanResult
                >>> runAppEventEmitRet
                >>> mockInterchangeEventLoad interchangeEvent
                >>> runDetermineDependenciesRet determineDependenciesResult
                >>> runPublishComponentResultRet @DependenciesDetermined
                >>> runPublishFailedMessageRet @RunPrepared
                >>> runIdentity

        let handlerResult = dependencyDeterminerHandler "asdf/asdf" receivedMessage
            (_, (dependenciesDetermined, (writtenDependencyRequests, (appEvents, (concludeSpanRequests, (startSpanRequests, _)))))) = runHandler handlerResult

        let expectedDependenciesDetermined =
              DependenciesDetermined
                { _basicRepoDependencies = determineDependenciesResult ^. #_basicRepoDependencies,
                  _ignoredRepoDependencies = determineDependenciesResult ^. #_ignoredRepoDependencies
                }

        annotate "produces the correct dependencies determined event"
        dependenciesDetermined === [SuccessComponentResult expectedDependenciesDetermined]

        annotate "emitted app events"
        genericLength @Integer appEvents === 2

        annotate "started and concluded a span"
        genericLength @Integer startSpanRequests === 1
        genericLength @Integer concludeSpanRequests === 1

        let expectedDetermineDependenciesRequest =
              DetermineDependenciesRequest
                { _programmingLanguages = V.singleton Python,
                  _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "repo-owner") (RepoName "repo-name"),
                  _commitSha = GitSha "head-sha",
                  _additionalDependencies = repoConfig ^. #_additionalDependencies,
                  _ignoreDependenciesConfig = repoConfig ^. #_ignoreDependenciesConfig,
                  _additionalDependencyFiles = repoConfig ^. #_additionalDependencyFiles
                }

        annotate "writes the correct write checks request"
        writtenDependencyRequests === [expectedDetermineDependenciesRequest]

    it "file parse failures produce a failed message" $
      hedgehog $ do
        run <- sample genRun
        receivedMessage <- sample genPubSubReceivedMessage
        ghInstallationAuth <- sample genGHInstallationAuth
        repoConfig <- sample genRepoConfig
        checkRun <- sample genCheckRun
        runTrace <- sample genRunTrace
        startSpanResult <- sample genStartSpanResult

        let massagedRun =
              run
                & (#_gitHeadSha . #_ntText) .~ "head-sha"
                & (#_qualifiedRepo . #_repoOwner . #_ntText) .~ "repo-owner"
                & (#_qualifiedRepo . #_repoName . #_ntText) .~ "repo-name"
                & (#_qualifiedRepo . #_repoHost) .~ GitHub
            interchangeEvent = InterchangeEvent ghInstallationAuth checkRun massagedRun repoConfig False (RunPrepared $ V.singleton Python)
            ddError = UnableToParseDependencyFile NpmPackageJson (GitPath "path") "boom"

        let runHandler =
              runIdentity
                . runPublishFailedMessageRet @RunPrepared
                . runPublishComponentResultRet @DependenciesDetermined
                . runError @DetermineDependenciesError
                . runDetermineDependenciesAlwaysThrow ddError
                . mockInterchangeEventLoad interchangeEvent
                . runAppEventEmitRet
                . runTraceEmitRet runTrace startSpanResult

        let handlerResult = dependencyDeterminerHandler "asdf/asdf" receivedMessage
            (publishedFailedMessages, (publishedCompontentResults, eitherTheRest)) = runHandler handlerResult

        let expectedProcessingError =
              ProcessingErrorUser $ UserErrorUnableToParseDependencyFile NpmPackageJson (GitPath "path") "boom"

        annotate "wrote the failed queue message"
        publishedFailedMessages === [FailedInterchangeEvent interchangeEvent]

        annotate "produces the correct dependencies determined event"
        publishedCompontentResults === [FailureComponentResult expectedProcessingError]

        annotate "Produces a final right"
        isRight eitherTheRest === True
