module Effect.ReadConfig.Backend.ParseConfigBackendSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.Dependency.File.DependenciesFileLoadDetails
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitFileMatch
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.RepoConfig.FileLoadPlan
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import Common.Model.RepoConfig.RepoConfig
import Common.Model.RepoConfig.Rules.FewYearlyCommitsConfig
import Common.Model.RepoConfig.Rules.NoRecentCommitConfig
import Common.Model.RepoConfig.Rules.NoRecentPackageConfig
import Common.Model.RepoConfig.Rules.RuleStatus
import Common.Model.RepoConfig.Rules.RulesConfig
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import RP.Effect.ReadConfig.Backend.ParseConfigBackend
import RP.Effect.ReadConfig.Model.ReadConfigError
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $ do
  context "When decoding config" $ do
    it "a happy day test" $ do
      let input =
            [r|
                    additional-dependency-files:
                      - path: 'custom/CustomPipfile'
                        type: pipenv-pipfile
                    additional-deps:
                      javascript:
                        - react
                        - react-dom
                        - '@testing-library/jest-dom'
                        - repo: commercialhaskell/stack
                          hosted-on: github # optional, defaults to 'github'
                    ignore-failures:
                      javascript:
                        - react-dom
                        - other-dep
                  |]
          expected =
            RepoConfig
              { _additionalDependencies =
                  V.fromList
                    [ BasicDependency JavaScript (DependencyIdentifierNamed (DependencyName "react")) Nothing,
                      BasicDependency JavaScript (DependencyIdentifierNamed (DependencyName "react-dom")) Nothing,
                      BasicDependency JavaScript (DependencyIdentifierNamed (DependencyName "@testing-library/jest-dom")) Nothing,
                      BasicDependency JavaScript (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "commercialhaskell") (RepoName "stack")) Nothing) Nothing
                    ],
                _ignoreDependenciesConfig =
                  IDSpecific $
                    V.singleton $
                      IgnoreLanguageDependencies JavaScript $
                        NV.unsafeFromList
                          [ DependencyName "react-dom",
                            DependencyName "other-dep"
                          ],
                _additionalDependencyFiles =
                  V.singleton (DependenciesFileLoad PipenvPipfile (DFLDSpecific "custom/CustomPipfile")),
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig = defaultRulesConfig
              }
          result = parseConfig input

      result `shouldBe` Right expected

    it "globbing is rejected" $ do
      let input =
            [r|
                    additional-dependency-files:
                      - path: ./**/other-dependencies*.txt
                        type: pip-requirements-txt
                  |]
          expected = ParseConfigFailed "Globbing is not supported for additional-dependency-files, you can use wildcards in the filename though."
          result = parseConfig input

      result `shouldBe` Left expected

    it "variations on directory + root search" $ do
      let input =
            [r|
                    additional-dependency-files:
                      - path: ./other-dependencies*.txt
                        type: pip-requirements-txt
                      - path: blah/other-dependencies*.txt
                        type: pip-requirements-txt
                      - path: wut/blah/other-dependencies*.txt
                        type: pip-requirements-txt
                      - path: 'custom/CustomPipfile'
                        type: pipenv-pipfile
                  |]
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles =
                  V.fromList
                    [ DependenciesFileLoad PipRequirementsTxt (DFLDDirectorySearch (GitPath "") (GitFileMatch "other-dependencies*.txt")),
                      DependenciesFileLoad PipRequirementsTxt (DFLDDirectorySearch (GitPath "blah") (GitFileMatch "other-dependencies*.txt")),
                      DependenciesFileLoad PipRequirementsTxt (DFLDDirectorySearch (GitPath "wut/blah") (GitFileMatch "other-dependencies*.txt")),
                      DependenciesFileLoad PipenvPipfile (DFLDSpecific "custom/CustomPipfile")
                    ],
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig = defaultRulesConfig
              }
          result = parseConfig input

      result `shouldBe` Right expected

    it "namespaced deps work" $ do
      let input =
            [r|
                    additional-deps:
                      java:
                        - com.google.cloud/bigquery
                        - name: org.hibernate/hibernate
                          repo: commercialhaskell/stack

                    ignore-failures:
                      php:
                        - psr/log
                  |]
          expected =
            RepoConfig
              { _additionalDependencies =
                  V.fromList
                    [ BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google.cloud/bigquery")) Nothing,
                      BasicDependency Java (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "commercialhaskell") (RepoName "stack")) (Just (DependencyName "org.hibernate/hibernate"))) Nothing
                    ],
                _ignoreDependenciesConfig =
                  IDSpecific $
                    V.singleton $
                      IgnoreLanguageDependencies Php $
                        NV.singleton $
                          DependencyName "psr/log",
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig = defaultRulesConfig
              }
          result = parseConfig input

      result `shouldBe` Right expected

    it "old style ignores work correctly" $ do
      let input =
            [r|
                    ignore-failures:
                      - react-dom
                      - other-dep
                  |]
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig =
                  IDAll $
                    V.fromList
                      [ DependencyName "react-dom",
                        DependencyName "other-dep"
                      ],
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig = defaultRulesConfig
              }
          result = parseConfig input

      result `shouldBe` Right expected

    it "missing fields are gracefully ignored" $ do
      let input = "nothing-interesting: ok"
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig = defaultRulesConfig
              }
          result = parseConfig input

      result `shouldBe` Right expected

    it "unknown programming language repo deps are gracefully handled" $ do
      let input =
            [r|
                    additional-deps:
                      fancy-pants-language:
                        - name: stack
                          repo: commercialhaskell/stack
                          hosted-on: github # optional, defaults to 'github'
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies =
                  V.singleton
                    (BasicDependency (UnsupportedLanguage "fancy-pants-language") (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "commercialhaskell") (RepoName "stack")) (Just (DependencyName "stack"))) Nothing),
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig = defaultRulesConfig
              }

      result `shouldBe` Right expected

    it "other additional deps must only be repo" $ do
      let input =
            [r|
                    additional-deps:
                      other:
                        - name: stack
                          repo: commercialhaskell/stack
                          hosted-on: github # optional, defaults to 'github'
                        - react
                  |]
          result = parseConfig input

      result `shouldBe` Left (ParseConfigFailed "Unsupported language other cannot have named dependency")

    it "decodes no recent package release" $ do
      let input =
            [r|
                    rules-config:
                      no-recent-package-release:
                        warn-at-months: 6
                        fail-at-months: 12
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _noRecentPackageConfig =
                        Just
                          NoRecentPackageConfig
                            { _warnAtMonths = Just 6,
                              _failAtMonths = Just 12
                            }
                    }
              }

      result `shouldBe` Right expected

    it "decodes no recent commit" $ do
      let input =
            [r|
                    rules-config:
                      no-recent-commit:
                        warn-at-months: 2
                        fail-at-months: 8
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _noRecentCommitConfig =
                        Just
                          NoRecentCommitConfig
                            { _warnAtMonths = Just 2,
                              _failAtMonths = Just 8
                            }
                    }
              }

      result `shouldBe` Right expected

    it "decodes no few yearly commits" $ do
      let input =
            [r|
                    rules-config:
                      few-yearly-commits:
                        warn-at-count: 9
                        fail-at-count: 3
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _fewYearlyCommitsConfig =
                        Just
                          FewYearlyCommitsConfig
                            { _warnAtCount = Just 9,
                              _failAtCount = Just 3
                            }
                    }
              }

      result `shouldBe` Right expected

    it "defaults fail at to Nothing" $ do
      let input =
            [r|
                    rules-config:
                      few-yearly-commits:
                        warn-at-count: 9
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _fewYearlyCommitsConfig =
                        Just
                          FewYearlyCommitsConfig
                            { _warnAtCount = Just 9,
                              _failAtCount = Nothing
                            }
                    }
              }

      result `shouldBe` Right expected

    it "defaulting is working for config" $ do
      let input =
            [r|
                    rules-config:
                      no-recent-package-release:
                        fail-at-months: 36
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _noRecentPackageConfig =
                        Just
                          NoRecentPackageConfig
                            { _warnAtMonths = Just 18,
                              _failAtMonths = Just 36
                            }
                    }
              }

      result `shouldBe` Right expected

    it "disabling is working for config at warn / error levels" $ do
      let input =
            [r|
                    rules-config:
                      no-recent-package-release:
                        warn-at-months: disabled
                        fail-at-months: 36
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _noRecentPackageConfig =
                        Just
                          NoRecentPackageConfig
                            { _warnAtMonths = Nothing,
                              _failAtMonths = Just 36
                            }
                    }
              }

      result `shouldBe` Right expected

    it "disabling all settings = parent rule is Nothing" $ do
      let input =
            [r|
                    rules-config:
                      no-recent-package-release:
                        warn-at-months: disabled
                        fail-at-months: disabled
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _noRecentPackageConfig = Nothing
                    }
              }

      result `shouldBe` Right expected

    it "disable a complex rule" $ do
      let input =
            [r|
                    rules-config:
                      no-recent-package-release: disabled
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _noRecentPackageConfig = Nothing
                    }
              }

      result `shouldBe` Right expected

    it "disable simple rules" $ do
      let input =
            [r|
                    rules-config:
                      repository-archived: disabled
                      repository-is-fork: disabled
                      package-deprecated: disabled
                      single-recent-author: disabled
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _repositoryArchivedRuleStatus = RSDisabled,
                      _repositoryIsForkRuleStatus = RSDisabled,
                      _packageDeprecatedRuleStatus = RSDisabled,
                      _singleRecentAuthorRuleStatus = RSDisabled
                    }
              }

      result `shouldBe` Right expected

    it "override simple rules to warn" $ do
      let input =
            [r|
                    rules-config:
                      repository-archived: warn
                      repository-is-fork: warn
                      package-deprecated: warn
                      single-recent-author: warn
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _repositoryArchivedRuleStatus = RSProduceWarning,
                      _repositoryIsForkRuleStatus = RSProduceWarning,
                      _packageDeprecatedRuleStatus = RSProduceWarning,
                      _singleRecentAuthorRuleStatus = RSProduceWarning
                    }
              }

      result `shouldBe` Right expected

    it "override simple rules to fail" $ do
      let input =
            [r|
                    rules-config:
                      repository-archived: fail
                      repository-is-fork: fail
                      package-deprecated: fail
                      single-recent-author: fail
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadEnabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _repositoryArchivedRuleStatus = RSProduceFailure,
                      _repositoryIsForkRuleStatus = RSProduceFailure,
                      _packageDeprecatedRuleStatus = RSProduceFailure,
                      _singleRecentAuthorRuleStatus = RSProduceFailure
                    }
              }

      result `shouldBe` Right expected

    it "loads disable auto load" $ do
      let input =
            [r|
                    disable-auto-file-load: true
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadDisabled,
                _rulesConfig =
                  defaultRulesConfig
                    { _repositoryArchivedRuleStatus = RSProduceFailure,
                      _repositoryIsForkRuleStatus = RSProduceWarning,
                      _packageDeprecatedRuleStatus = RSProduceFailure,
                      _singleRecentAuthorRuleStatus = RSProduceWarning
                    }
              }

      result `shouldBe` Right expected

    it "loads disable auto load with languages" $ do
      let input =
            [r|
                    disable-auto-file-load:
                      - javascript
                      - ruby
                  |]
          result = parseConfig input
          expected =
            RepoConfig
              { _additionalDependencies = V.empty,
                _ignoreDependenciesConfig = IDSpecific V.empty,
                _additionalDependencyFiles = V.empty,
                _fileLoadPlan = FileLoadDisabledForLangs (NV.unsafeFromList [JavaScript, Ruby]),
                _rulesConfig =
                  defaultRulesConfig
                    { _repositoryArchivedRuleStatus = RSProduceFailure,
                      _repositoryIsForkRuleStatus = RSProduceWarning,
                      _packageDeprecatedRuleStatus = RSProduceFailure,
                      _singleRecentAuthorRuleStatus = RSProduceWarning
                    }
              }

      result `shouldBe` Right expected
