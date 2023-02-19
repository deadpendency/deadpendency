{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Error where

import Common.Model.Error.ProcessingError
import Common.Model.Error.UserError
import CommonTest.Gen.General
import CommonTest.Gen.Model.Dependency
import CommonTest.Gen.Model.Git
import CommonTest.Gen.Model.Report
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genProcessingError :: Gen ProcessingError
genProcessingError =
  Gen.choice
    [ Gen.constant ProcessingErrorApplication,
      ProcessingErrorUser <$> genUserError
    ]

genUserError :: Gen UserError
genUserError =
  Gen.choice
    [ UserErrorInvalidConfig <$> genAlphaText,
      Gen.constant UserErrorIgnoredAllDependencies,
      Gen.constant UserErrorNoDependenciesFound,
      UserErrorTooManyDependencies <$> genPositiveInt,
      UserErrorInvalidDependencyFile <$> genDependenciesFileType <*> genGitPath <*> genAlphaText,
      UserErrorAllDepsFailedFetch <$> genNonEmptyVector (Range.constant 1 10) (genDependencyLanguageReport genDependencyErrorReports)
    ]
