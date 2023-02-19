module RG.Effect.AssessDependencies.Backend.Rules.IsPackageNotAliveRule
  ( isPackageNotAliveRule,
  )
where

import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import RG.Effect.AssessDependencies.Backend.Model.InternalDependencyAssessment
import RG.Effect.AssessDependencies.Backend.Rules.Rule (Rule)

isPackageNotAliveRule :: Rule
isPackageNotAliveRule _ RulesConfig {_packageDeprecatedRuleStatus = depStatus} _ theseRegistryRepo =
  case (depStatus, theseRegistryRepo ^? here) of
    (RSDisabled, _) -> InternalDependencyAssessment V.empty V.empty
    (_, Nothing) -> InternalDependencyAssessment V.empty V.empty
    (RSProduceWarning, Just registryInfo) ->
      InternalDependencyAssessment (DependencyAssessmentWarning <$> produceViolation registryInfo) V.empty
    (RSProduceFailure, Just registryInfo) ->
      InternalDependencyAssessment V.empty (DependencyAssessmentFailure <$> produceViolation registryInfo)

produceViolation :: DependencyRegistryInfo -> V.Vector DependencyAssessmentViolation
produceViolation registryInfo =
  let aliveness = registryInfo ^. #_alivenessStatus
      registry = registryInfo ^. #_registry
   in case aliveness of
        (RASDeprecated alivenessStatusType maybeDeprecationMessage deprecatedForNames) ->
          let davdtAlivenessType =
                case alivenessStatusType of
                  RASTDeprecated -> DAVDTDeprecated
                  RASTAbandoned -> DAVDTAbandoned
                  RASTRelocated -> DAVDTRelocated
           in V.singleton $ DAVPackageDeprecated registry davdtAlivenessType maybeDeprecationMessage deprecatedForNames
        _ -> V.empty
