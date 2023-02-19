module Common.Model.RepoConfig.Rules.RuleStatus
  ( RuleStatus (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data RuleStatus
  = RSDisabled
  | RSProduceWarning
  | RSProduceFailure
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON RuleStatus where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RuleStatus where
  parseJSON = genericParseJSON cleanJSONOptions
