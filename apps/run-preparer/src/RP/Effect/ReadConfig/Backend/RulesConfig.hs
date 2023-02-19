{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fno-warn-orphans #-}

module RP.Effect.ReadConfig.Backend.RulesConfig () where

import Common.Model.RepoConfig.Rules.FewYearlyCommitsConfig
import Common.Model.RepoConfig.Rules.NoRecentCommitConfig
import Common.Model.RepoConfig.Rules.NoRecentPackageConfig
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Common.Parsing.HsYAML ()
import Data.YAML

{-
rules-config:
  no-recent-package-release:
    warn-at-months: 18
    fail-at-months: 24
  no-recent-commit:
    warn-at-months: 12
    fail-at-months: 18
  few-yearly-commits:
    warn-at-count: 2
    fail-at-count: disabled # does not fail by default

  # these can be 'disabled', 'warn' or 'fail'
  repository-archived: fail
  repository-is-fork: warn
  package-deprecated: fail
  single-recent-author: warn
  repository-not-identified: warn
  repository-not-found: warn
-}

instance FromYAML RulesConfig where
  parseYAML =
    withMap "RulesConfig" $
      \m ->
        RulesConfig
          <$> m .:? "no-recent-package-release" .!= Just defaultNoRecentPackageConfig
          <*> m .:? "no-recent-commit" .!= Just defaultNoRecentCommitConfig
          <*> m .:? "few-yearly-commits" .!= Just defaultFewYearlyCommitsConfig
          <*> m .:? "repository-archived" .!= RSProduceFailure
          <*> m .:? "repository-is-fork" .!= RSProduceWarning
          <*> m .:? "package-deprecated" .!= RSProduceFailure
          <*> m .:? "single-recent-author" .!= RSProduceWarning
          <*> m .:? "repository-not-identified" .!= RSProduceWarning
          <*> m .:? "repository-not-found" .!= RSProduceWarning

instance FromYAML RuleStatus where
  parseYAML =
    withStr "RuleStatus" $ \s ->
      let lowerS = toLower s
       in case lowerS of
            "disabled" -> pure RSDisabled
            "warn" -> pure RSProduceWarning
            "fail" -> pure RSProduceFailure
            other -> fail $ "Unexpected rule status of: '" <> unpack other <> "'. Expected disabled, warn or fail."

instance {-# OVERLAPPING #-} FromYAML (Maybe NoRecentPackageConfig) where
  parseYAML (Scalar _ (SStr str)) =
    if toLower str == "disabled"
      then pure Nothing
      else fail $ "Unexpected string for no-recent-package-release: " <> unpack str
  parseYAML (Mapping _ _ m) = do
    maybeWarnAtMonths <- defaultToValue "warn-at-months" (defaultNoRecentPackageConfig ^. #_warnAtMonths) m
    maybeErrorAtMonths <- defaultToValue "fail-at-months" (defaultNoRecentPackageConfig ^. #_failAtMonths) m
    pure $
      case (maybeWarnAtMonths, maybeErrorAtMonths) of
        (Nothing, Nothing) -> Nothing
        (warnAt, errorAt) -> Just $ NoRecentPackageConfig warnAt errorAt
  parseYAML mismatched = typeMismatch "NoRecentPackageConfig" mismatched

instance {-# OVERLAPPING #-} FromYAML (Maybe NoRecentCommitConfig) where
  parseYAML (Scalar _ (SStr str)) =
    if toLower str == "disabled"
      then pure Nothing
      else fail $ "Unexpected string for no-recent-commit: " <> unpack str
  parseYAML (Mapping _ _ m) = do
    maybeWarnAtMonths <- defaultToValue "warn-at-months" (defaultNoRecentCommitConfig ^. #_warnAtMonths) m
    maybeErrorAtMonths <- defaultToValue "fail-at-months" (defaultNoRecentCommitConfig ^. #_failAtMonths) m
    pure $
      case (maybeWarnAtMonths, maybeErrorAtMonths) of
        (Nothing, Nothing) -> Nothing
        (warnAt, errorAt) -> Just $ NoRecentCommitConfig warnAt errorAt
  parseYAML mismatched = typeMismatch "NoRecentCommitConfig" mismatched

instance {-# OVERLAPPING #-} FromYAML (Maybe FewYearlyCommitsConfig) where
  parseYAML (Scalar _ (SStr str)) =
    if toLower str == "disabled"
      then pure Nothing
      else fail $ "Unexpected string for few-yearly-commits: " <> unpack str
  parseYAML (Mapping _ _ m) = do
    maybeWarnAtCount <- defaultToValue "warn-at-count" (defaultFewYearlyCommitsConfig ^. #_warnAtCount) m
    maybeErrorAtCount <- defaultToValue "fail-at-count" (defaultFewYearlyCommitsConfig ^. #_failAtCount) m
    pure $
      case (maybeWarnAtCount, maybeErrorAtCount) of
        (Nothing, Nothing) -> Nothing
        (warnAt, errorAt) -> Just $ FewYearlyCommitsConfig warnAt errorAt
  parseYAML mismatched = typeMismatch "FewYearlyCommitsConfig" mismatched

defaultToValue :: Text -> Maybe Int -> Mapping Pos -> Parser (Maybe Int)
defaultToValue keyName defaultValue m = do
  result <- m .:? keyName
  case result of
    (Just (Scalar _ (SInt theInt))) -> pure $ Just $ fromIntegral theInt
    (Just (Scalar _ (SStr str))) ->
      if toLower str == "disabled"
        then pure Nothing
        else case readEither @Int (unpack str) of
          Right theInt -> pure $ Just theInt
          Left failure -> fail $ "Unexpected value for: " <> unpack keyName <> " producing error: " <> unpack failure
    (Just other) -> typeMismatch (unpack keyName) other
    Nothing -> pure defaultValue
