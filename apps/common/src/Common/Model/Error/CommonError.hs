module Common.Model.Error.CommonError
  ( CommonError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.ConsideredAppFailure
import Data.Aeson

data CommonError
  = NonRetryableFailure
  | GoogleEnvLoadError
  | CommonConfigLoadError
  | UnexpectedRunMissing
  | PubSubPublishQueueIdConfigMissing
  | PubSubFailureQueueIdConfigMissing
  | PubSubDlqQueueIdConfigMissing
  | PubSubAttemptPublishMissingFailedMessage
  | PubSubMessageMissing
  | PubSubMessageBytestringDecodeFailure Text Text
  | PubSubMessageToJSONDecodeFailure Text
  | GitHubInteractError Text
  | GitHubResponseDecodeError Text
  | GitHubAppAuthCreationError Text
  | GitHubUnexpectedNotFound Text
  | GetInstallAuthBeforePut
  | GetRepoConfigBeforePut
  | GetRunBeforePut
  | GetCheckRunBeforePut
  | GetRunTraceBeforePut
  | CacheError Text
  | UnexpectedEmptyDependenciesInStream
  deriving stock (Eq, Show, Generic)

instance ConsideredAppFailure CommonError where
  consideredAppFailure _ = True

instance ToJSON CommonError where
  toJSON = genericToJSON cleanJSONOptions
