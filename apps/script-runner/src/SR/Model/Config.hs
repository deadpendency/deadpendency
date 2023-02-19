module SR.Model.Config
  ( Config (..),
  )
where

import Common.Model.GitHub.GHAppId

data Config = Config
  { _initQueueTopicId :: Text,
    _initQueueDlqSubIdA :: Text,
    _initQueueDlqSubIdB :: Text,
    _checkRunCreatedQueueTopicId :: Text,
    _checkRunCreatedQueueDlqSubIdA :: Text,
    _checkRunCreatedQueueDlqSubIdB :: Text,
    _runPreparedQueueTopicId :: Text,
    _runPreparedQueueDlqSubIdA :: Text,
    _runPreparedQueueDlqSubIdB :: Text,
    _depsDeterminedQueueTopicId :: Text,
    _depsDeterminedQueueDlqSubIdA :: Text,
    _depsDeterminedQueueDlqSubIdB :: Text,
    _depsFetchedQueueTopicId :: Text,
    _depsFetchedQueueDlqSubIdA :: Text,
    _depsFetchedQueueDlqSubIdB :: Text,
    _reportGeneratedQueueTopicId :: Text,
    _reportGeneratedQueueDlqSubIdA :: Text,
    _reportGeneratedQueueDlqSubIdB :: Text,
    _githubPrivateKeySecretName :: Text,
    _appId :: GHAppId
  }
  deriving stock (Generic)
