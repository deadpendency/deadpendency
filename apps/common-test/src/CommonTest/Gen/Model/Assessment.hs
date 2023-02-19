{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Assessment where

import Common.Model.Assessment.DependencyAssessment
import Common.Model.Assessment.DependencyAssessmentFailure
import Common.Model.Assessment.DependencyAssessmentResult
import Common.Model.Assessment.DependencyAssessmentViolation
import Common.Model.Assessment.DependencyAssessmentWarning
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Ecosystem
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genDependencyAssessment :: Gen DependencyAssessment
genDependencyAssessment =
  DependencyAssessment
    <$> genEnrichedDependency
    <*> genDependencyAssessmentResult

genDependencyAssessmentResult :: Gen DependencyAssessmentResult
genDependencyAssessmentResult =
  Gen.choice
    [ genDependencyAssessmentResultPass,
      genDependencyAssessmentResultWarning,
      genDependencyAssessmentResultFailure
    ]

genDependencyAssessmentResultPass :: Gen DependencyAssessmentResult
genDependencyAssessmentResultPass = Gen.constant DARPass

genDependencyAssessmentResultWarning :: Gen DependencyAssessmentResult
genDependencyAssessmentResultWarning =
  DARWarning <$> genNonEmptyVector (Range.constant 1 10) genDependencyAssessmentWarning

genDependencyAssessmentResultFailure :: Gen DependencyAssessmentResult
genDependencyAssessmentResultFailure =
  DARFailure
    <$> genNonEmptyVector (Range.constant 1 10) genDependencyAssessmentFailure
    <*> genVector (Range.constant 1 10) genDependencyAssessmentWarning

genDependencyAssessmentWarning :: Gen DependencyAssessmentWarning
genDependencyAssessmentWarning = DependencyAssessmentWarning <$> genDependencyAssessmentViolation

genDependencyAssessmentFailure :: Gen DependencyAssessmentFailure
genDependencyAssessmentFailure = DependencyAssessmentFailure <$> genDependencyAssessmentViolation

genDependencyAssessmentViolation :: Gen DependencyAssessmentViolation
genDependencyAssessmentViolation =
  Gen.choice
    [ DAVNoRecentCommits <$> genPositiveInt <*> Gen.maybe genUTCTime,
      DAVFewYearlyCommits <$> genPositiveInt <*> genPositiveInt,
      DAVNoRecentPackageRelease <$> genPositiveInt <*> genUTCTime,
      Gen.constant DAVSingleRecentAuthor,
      DAVPackageDeprecated <$> genRegistry <*> genDAVDeprecationType <*> Gen.maybe genAlphaText <*> genVector (Range.constant 0 10) genDependencyName,
      Gen.constant DAVIsFork,
      Gen.constant DAVRepoArchived,
      Gen.constant DAVRepoNotIdentified,
      Gen.constant DAVRepoNotFound
    ]

genDAVDeprecationType :: Gen DAVDeprecationType
genDAVDeprecationType = Gen.enumBounded
