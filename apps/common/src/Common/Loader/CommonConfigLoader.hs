module Common.Loader.CommonConfigLoader (loadCommonConfig) where

import Common.Model.Config.AppEnv
import Common.Model.Config.AppVersion
import Common.Model.Config.CommonConfig
import Relude.Lifted.Env (lookupEnv)
import System.Environment (getEnv)
import System.IO.Error (ioError, userError)

loadCommonConfig :: IO CommonConfig
loadCommonConfig = do
  putTextLn "Loading Common Config"
  port <- flip whenNothingM (ioError (userError "Error - PORT env variable not an integer")) $ getEnv "PORT" <&> readMaybe
  cloudRunServiceName <- toText <$> getEnv "K_SERVICE"
  cloudRunRevision <- toText <$> getEnv "K_REVISION"
  cloudRunConfiguration <- toText <$> getEnv "K_CONFIGURATION"
  downstreamPubSubQueueId <- toText <<$>> lookupEnv "DOWNSTREAM_QUEUE"
  dlqPubSubQueueId <- toText <<$>> lookupEnv "DEAD_LETTER_QUEUE"
  failurePubSubQueueId <- toText <<$>> lookupEnv "FAILURE_QUEUE"
  appEnv <- flip whenNothingM (ioError (userError "Error - Unexpected AppEnv")) $ getAppEnv . toText <$> getEnv "APP_ENV"
  appVersion <- AppVersion . toText <$> getEnv "APP_VERSION"
  let commonConfig =
        CommonConfig
          { _port = port,
            _cloudRunServiceName = cloudRunServiceName,
            _cloudRunRevision = cloudRunRevision,
            _cloudRunConfiguration = cloudRunConfiguration,
            _downstreamPubSubQueueId = downstreamPubSubQueueId,
            _dlqPubSubQueueId = dlqPubSubQueueId,
            _failurePubSubQueueId = failurePubSubQueueId,
            _appEnv = appEnv,
            _appVersion = appVersion
          }
  putTextLn $ "Common Config Loaded: " <> show commonConfig
  pure commonConfig

getAppEnv :: Text -> Maybe AppEnv
getAppEnv "prod" = Just Prod
getAppEnv "preprod" = Just PreProd
getAppEnv _ = Nothing
