{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Ecosystem where

import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Ecosystem.Registry
import CommonTest.Gen.General
import Hedgehog
import Hedgehog.Gen qualified as Gen

genProgrammingLanguage :: Gen ProgrammingLanguage
genProgrammingLanguage =
  Gen.choice
    [ Gen.constant JavaScript,
      Gen.constant TypeScript,
      Gen.constant Python,
      Gen.constant Php,
      Gen.constant Ruby,
      Gen.constant Haskell,
      Gen.constant Rust,
      Gen.constant CSharpNet,
      Gen.constant VisualBasicNet,
      Gen.constant Java,
      Gen.constant Kotlin,
      Gen.constant Golang,
      UnsupportedLanguage <$> genAlphaText
    ]

genRegistry :: Gen Registry
genRegistry = Gen.enumBounded
