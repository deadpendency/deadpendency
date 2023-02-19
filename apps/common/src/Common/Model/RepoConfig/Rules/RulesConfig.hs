module Common.Model.RepoConfig.Rules.RulesConfig
  ( RulesConfig (..),
    defaultRulesConfig,
  )
where

import Common.Aeson.Aeson
import Common.Model.RepoConfig.Rules.FewYearlyCommitsConfig
import Common.Model.RepoConfig.Rules.NoRecentCommitConfig
import Common.Model.RepoConfig.Rules.NoRecentPackageConfig
import Common.Model.RepoConfig.Rules.RuleStatus
import Data.Aeson

data RulesConfig = RulesConfig
  { _noRecentPackageConfig :: Maybe NoRecentPackageConfig,
    _noRecentCommitConfig :: Maybe NoRecentCommitConfig,
    _fewYearlyCommitsConfig :: Maybe FewYearlyCommitsConfig,
    _repositoryArchivedRuleStatus :: RuleStatus,
    _repositoryIsForkRuleStatus :: RuleStatus,
    _packageDeprecatedRuleStatus :: RuleStatus,
    _singleRecentAuthorRuleStatus :: RuleStatus,
    _repositoryNotIdentified :: RuleStatus,
    _repositoryNotFound :: RuleStatus
  }
  deriving stock (Eq, Show, Generic)

defaultRulesConfig :: RulesConfig
defaultRulesConfig =
  RulesConfig
    { _noRecentPackageConfig = Just defaultNoRecentPackageConfig,
      _noRecentCommitConfig = Just defaultNoRecentCommitConfig,
      _fewYearlyCommitsConfig = Just defaultFewYearlyCommitsConfig,
      _repositoryArchivedRuleStatus = RSProduceFailure,
      _repositoryIsForkRuleStatus = RSProduceWarning,
      _packageDeprecatedRuleStatus = RSProduceFailure,
      _singleRecentAuthorRuleStatus = RSProduceWarning,
      _repositoryNotIdentified = RSProduceWarning,
      _repositoryNotFound = RSProduceWarning
    }

instance ToJSON RulesConfig where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RulesConfig where
  parseJSON = genericParseJSON cleanJSONOptions
