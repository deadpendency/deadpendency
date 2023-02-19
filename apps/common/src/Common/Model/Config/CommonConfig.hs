module Common.Model.Config.CommonConfig
  ( CommonConfig (..),
  )
where

import Common.Model.Config.AppEnv
import Common.Model.Config.AppVersion

data CommonConfig = CommonConfig
  { _port :: Int,
    _cloudRunServiceName :: Text,
    _cloudRunRevision :: Text,
    _cloudRunConfiguration :: Text,
    _downstreamPubSubQueueId :: Maybe Text,
    _dlqPubSubQueueId :: Maybe Text,
    _failurePubSubQueueId :: Maybe Text,
    _appEnv :: AppEnv,
    _appVersion :: AppVersion
  }
  deriving stock (Show, Generic)
