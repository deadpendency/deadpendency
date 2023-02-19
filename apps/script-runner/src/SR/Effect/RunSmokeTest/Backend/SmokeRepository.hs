module SR.Effect.RunSmokeTest.Backend.SmokeRepository (smokeRepository) where

import Common.GitHub.GetCheckRunOutput.Model.GetCheckRunOutputRequest
import Common.GitHub.TriggerCheckSuite.TriggerCheckSuite
import Common.HtmlReport.HtmlReport
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppId
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad.Except (liftEither, throwError)
import Data.Algorithm.Diff qualified as D
import Data.Algorithm.DiffOutput qualified as D
import Data.List qualified as L
import Data.Text.Lazy qualified as TL
import SR.Effect.RunSmokeTest.Backend.Common
import SR.Effect.RunSmokeTest.Backend.Model.SmokeConfiguration
import SR.Effect.RunSmokeTest.Model.SmokeResult
import Text.Pretty.Simple (pShow)

smokeRepository :: GHAppId -> GHInstallationAuth -> SmokeConfiguration -> IO SmokeResult
smokeRepository appId installAuth sc = do
  let repoNodeId = sc ^. #_repoNodeId
      checkSuiteNodeId = sc ^. #_checkSuiteNodeId
      qualifiedRepo = sc ^. #_repo
      commitSha = sc ^. #_commitSha
      expectedDeps = sc ^. #_expectedDeps
      expectedErrors = sc ^. #_expectedErrors

  eitherResult <-
    runExceptT $ do
      checkRunNodeId <- ExceptT $ firstF ((<>) "Failed to retrigger check run: " . show) $ githubTriggerCheckSuite installAuth repoNodeId checkSuiteNodeId

      -- delay 30 seconds to allow processing to complete
      lift $ threadDelay 30000000

      -- poll check run every second for update
      let request =
            GetCheckRunOutputRequest
              { _appId = appId ^. #_ntInt,
                _qualifiedRepo = qualifiedRepo,
                _commitSha = commitSha
              }

      result <- ExceptT $ firstF ((<>) "Failed to fetch report: " . show) $ race (threadDelay 120000000) (pollForNewCheckRunCompleted checkRunNodeId installAuth request)

      -- convert the result to an overall report
      checkRunOutput <-
        case result ^. #_output of
          Just output' -> pure output'
          _ -> throwError "Report is missing output"

      let textualReport = checkRunOutputToHtmlReportTextual checkRunOutput

      overallReport <- liftEither $ first ((<>) "Unexpected failure to parse report: " . show) $ fromHtmlReport textualReport

      let depList = getDepList overallReport
          errorList = getErroredDepList overallReport
          depDiffFailure = getDiff expectedDeps depList
          errorDiffFailure = getDiff expectedErrors errorList

      pure $
        case (depDiffFailure, errorDiffFailure) of
          (Just depFailure, Just errorFailure) -> SRFailure qualifiedRepo (These (DepsMismatch depFailure) (ErrorsMismatch errorFailure))
          (Just depFailure, Nothing) -> SRFailure qualifiedRepo (This (DepsMismatch depFailure))
          (Nothing, Just errorFailure) -> SRFailure qualifiedRepo (That (ErrorsMismatch errorFailure))
          (Nothing, Nothing) -> SRSuccess

  pure $
    case eitherResult of
      Right smokeResult -> smokeResult
      Left errorText -> SRException qualifiedRepo errorText

getDiff :: [DependencyIdentifier] -> [DependencyIdentifier] -> Maybe Text
getDiff expected result =
  let diffResult = D.getDiff (pShowString <$> sort expected) (pShowString <$> sort result)
   in if L.all isBoth diffResult
        then Nothing
        else Just $ stripStart $ pack $ D.ppDiff diffResult

isBoth :: D.Diff [String] -> Bool
isBoth (D.Both _ _) = True
isBoth _ = False

pShowString :: (Show a) => a -> [String]
pShowString = fmap toString . TL.lines . pShow
