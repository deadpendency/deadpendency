module RG.Effect.AssessDependencies.Backend.Rules.Rules
  ( rules,
  )
where

import Data.Vector.NonEmpty qualified as NV
import RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.FewYearlyCommitsRule
import RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.NoRecentCommitsRule
import RG.Effect.AssessDependencies.Backend.Rules.CommitHealth.SingleRecentAuthorRule
import RG.Effect.AssessDependencies.Backend.Rules.IsArchivedRule
import RG.Effect.AssessDependencies.Backend.Rules.IsForkRule
import RG.Effect.AssessDependencies.Backend.Rules.IsPackageNotAliveRule
import RG.Effect.AssessDependencies.Backend.Rules.IsRepoNotFoundRule
import RG.Effect.AssessDependencies.Backend.Rules.IsRepoNotIdentifiedRule
import RG.Effect.AssessDependencies.Backend.Rules.NoRecentPackageReleaseRule
import RG.Effect.AssessDependencies.Backend.Rules.Rule

rules :: NV.NonEmptyVector Rule
rules =
  NV.singleton noRecentCommitsRule
    `NV.snoc` fewYearlyCommitsRule
    `NV.snoc` singleRecentAuthorRule
    `NV.snoc` isArchivedRule
    `NV.snoc` isForkRule
    `NV.snoc` isPackageNotAliveRule
    `NV.snoc` noRecentPackageReleaseRule
    `NV.snoc` isRepoNotIdentifiedRule
    `NV.snoc` isRepoNotFoundRule
