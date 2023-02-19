module SR.Loader.ConfigLoader
  ( loadConfig,
  )
where

import Common.Model.GitHub.GHAppId
import SR.Model.Config (Config (..))
import System.Environment (getEnv)
import System.IO.Error (ioError, userError)

loadConfig :: IO Config
loadConfig = do
  putTextLn "Loading App Config"
  initQueue <- toText <$> getEnv "INIT_QUEUE"
  initQueueDlqSubIdA <- toText <$> getEnv "INIT_QUEUE_DLQ_SUB_ID_A"
  initQueueDlqSubIdB <- toText <$> getEnv "INIT_QUEUE_DLQ_SUB_ID_B"
  checkRunCreatedQueue <- toText <$> getEnv "CHECK_RUN_CREATED_QUEUE"
  checkRunCreatedQueueDlqSubIdA <- toText <$> getEnv "CHECK_RUN_CREATED_QUEUE_DLQ_SUB_ID_A"
  checkRunCreatedQueueDlqSubIdB <- toText <$> getEnv "CHECK_RUN_CREATED_QUEUE_DLQ_SUB_ID_B"
  runPreparedQueue <- toText <$> getEnv "RUN_PREPARED_QUEUE"
  runPreparedQueueDlqSubIdA <- toText <$> getEnv "RUN_PREPARED_QUEUE_DLQ_SUB_ID_A"
  runPreparedQueueDlqSubIdB <- toText <$> getEnv "RUN_PREPARED_QUEUE_DLQ_SUB_ID_B"
  depsDeterminedQueue <- toText <$> getEnv "DEPENDENCIES_DETERMINED_QUEUE"
  depsDeterminedQueueDlqSubIdA <- toText <$> getEnv "DEPENDENCIES_DETERMINED_QUEUE_DLQ_SUB_ID_A"
  depsDeterminedQueueDlqSubIdB <- toText <$> getEnv "DEPENDENCIES_DETERMINED_QUEUE_DLQ_SUB_ID_B"
  depsFetchedQueue <- toText <$> getEnv "DEPENDENCIES_FETCHED_QUEUE"
  depsFetchedQueueDlqSubIdA <- toText <$> getEnv "DEPENDENCIES_FETCHED_QUEUE_DLQ_SUB_ID_A"
  depsFetchedQueueDlqSubIdB <- toText <$> getEnv "DEPENDENCIES_FETCHED_QUEUE_DLQ_SUB_ID_B"
  reportGeneratedQueue <- toText <$> getEnv "REPORT_GENERATED_QUEUE"
  reportGeneratedQueueDlqSubIdA <- toText <$> getEnv "REPORT_GENERATED_QUEUE_DLQ_SUB_ID_A"
  reportGeneratedQueueDlqSubIdB <- toText <$> getEnv "REPORT_GENERATED_QUEUE_DLQ_SUB_ID_B"
  gitHubKey <- toText <$> getEnv "GITHUB_PRIVATE_KEY_SECRET_NAME"
  appId <- flip whenNothingM (ioError (userError "Error - APP_ID env variable not an integer")) $ getEnv "APP_ID" <&> readMaybe
  let config =
        Config
          { _initQueueTopicId = initQueue,
            _initQueueDlqSubIdA = initQueueDlqSubIdA,
            _initQueueDlqSubIdB = initQueueDlqSubIdB,
            _checkRunCreatedQueueTopicId = checkRunCreatedQueue,
            _checkRunCreatedQueueDlqSubIdA = checkRunCreatedQueueDlqSubIdA,
            _checkRunCreatedQueueDlqSubIdB = checkRunCreatedQueueDlqSubIdB,
            _runPreparedQueueTopicId = runPreparedQueue,
            _runPreparedQueueDlqSubIdA = runPreparedQueueDlqSubIdA,
            _runPreparedQueueDlqSubIdB = runPreparedQueueDlqSubIdB,
            _depsDeterminedQueueTopicId = depsDeterminedQueue,
            _depsDeterminedQueueDlqSubIdA = depsDeterminedQueueDlqSubIdA,
            _depsDeterminedQueueDlqSubIdB = depsDeterminedQueueDlqSubIdB,
            _depsFetchedQueueTopicId = depsFetchedQueue,
            _depsFetchedQueueDlqSubIdA = depsFetchedQueueDlqSubIdA,
            _depsFetchedQueueDlqSubIdB = depsFetchedQueueDlqSubIdB,
            _reportGeneratedQueueTopicId = reportGeneratedQueue,
            _reportGeneratedQueueDlqSubIdA = reportGeneratedQueueDlqSubIdA,
            _reportGeneratedQueueDlqSubIdB = reportGeneratedQueueDlqSubIdB,
            _githubPrivateKeySecretName = gitHubKey,
            _appId = GHAppId appId
          }
  putTextLn "App Config Loaded"
  pure config
