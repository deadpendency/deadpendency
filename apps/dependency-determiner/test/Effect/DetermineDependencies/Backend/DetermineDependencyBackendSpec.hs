module Effect.DetermineDependencies.Backend.DetermineDependencyBackendSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Dependency.Ignored.IgnoredDependency
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.RepoConfig.IgnoreDependenciesConfig
import CommonTest.Gen.Model.Git
import DD.Effect.DetermineDependencies.Backend.DetermineDependencyBackend
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Hedgehog.Gen (sample)
import Test.Hspec

spec :: Spec
spec = parallel $
  context "DetermineDependencyBackendSpec" $ do
    context "ignore deps" $ do
      it "ignores happy day dependencies" $ do
        let basicDependencies =
              NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                `NV.snoc` BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
            ignoreDependenciesConfig =
              IDAll $ V.singleton (DependencyName "dep-2")
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies $ V.singleton (IgnoredDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing),
                Just $ NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
              )

        result `shouldBe` expected

      it "deps with namespace also works" $ do
        let basicDependencies =
              NV.singleton (BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing)
                `NV.snoc` BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigtable")) Nothing
                `NV.snoc` BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.bloogle/bigquery")) Nothing
            ignoreDependenciesConfig =
              IDAll $ V.singleton (DependencyName "com.google/bigquery")
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies $ V.singleton (IgnoredDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing),
                Just $
                  NV.unsafeFromList
                    [ BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigtable")) Nothing,
                      BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.bloogle/bigquery")) Nothing
                    ]
              )

        result `shouldBe` expected

      it "ignores language specific dependencies" $ do
        let basicDependencies =
              NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                `NV.snoc` BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
                `NV.snoc` BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
            ignoreDependenciesConfig =
              IDSpecific $
                V.singleton $
                  IgnoreLanguageDependencies Haskell $
                    NV.unsafeFromList
                      [ DependencyName "dep-1",
                        DependencyName "dep-2"
                      ]
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies $ V.singleton (IgnoredDependency Haskell (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing),
                Just $
                  NV.unsafeFromList
                    [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing,
                      BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
                    ]
              )

        result `shouldBe` expected

      it "ignores language specific namespaced dependencies" $ do
        let basicDependencies =
              NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing)
                `NV.snoc` BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing
            ignoreDependenciesConfig =
              IDSpecific $
                V.singleton $
                  IgnoreLanguageDependencies Java $
                    NV.unsafeFromList
                      [ DependencyName "com.google/bigquery"
                      ]
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies $ V.singleton (IgnoredDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing),
                Just $
                  NV.unsafeFromList
                    [ BasicDependency JavaScript (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing
                    ]
              )

        result `shouldBe` expected

      it "missing deps don't appear as ignored" $ do
        let basicDependencies =
              NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                `NV.snoc` BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
            ignoreDependenciesConfig =
              IDAll $ V.singleton (DependencyName "dep-3")
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies V.empty,
                Just $
                  NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                    `NV.snoc` BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
              )

        result `shouldBe` expected

      it "all deps ignored results in no basic dep result" $ do
        let basicDependencies =
              NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                `NV.snoc` BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing
            ignoreDependenciesConfig =
              IDAll $
                V.singleton (DependencyName "dep-1")
                  `V.snoc` DependencyName "dep-2"
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies $
                  V.singleton (IgnoredDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                    `V.snoc` IgnoredDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-2") Nothing,
                Nothing
              )

        result `shouldBe` expected

      it "repo deps can also be ignored" $ do
        qualifiedRepo <- sample genQualifiedRepo
        let basicDependencies =
              NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
                `NV.snoc` BasicDependency JavaScript (DependencyIdentifierRepo qualifiedRepo (Just $ DependencyName "dep-2")) Nothing
            ignoreDependenciesConfig =
              IDAll $ V.singleton (DependencyName "dep-2")
            result = ignoreDeps ignoreDependenciesConfig basicDependencies
            expected =
              ( IgnoredRepoDependencies $
                  V.singleton (IgnoredDependency JavaScript (DependencyIdentifierRepo qualifiedRepo (Just $ DependencyName "dep-2")) Nothing),
                Just $ NV.singleton (BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing)
              )

        result `shouldBe` expected

    context "remove duplicates" $ do
      it "removes happy day duplicates" $ do
        let basicDependencies =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing,
                  BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing
                ]
            result = removeDuplicates basicDependencies
            expected =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing
                ]

        result `shouldBe` expected

      it "removes happy day namespaced duplicates" $ do
        let basicDependencies =
              V.fromList
                [ BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing,
                  BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing
                ]
            result = removeDuplicates basicDependencies
            expected =
              V.fromList
                [ BasicDependency Java (DependencyIdentifierNamed (DependencyName "com.google/bigquery")) Nothing
                ]

        result `shouldBe` expected

      it "dependency type doesn't make a difference" $ do
        let basicDependencies =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing,
                  BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") (Just CoreDependency)
                ]
            result = removeDuplicates basicDependencies
            expected =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing
                ]

        result `shouldBe` expected

      it "repo deps will also avoid dups" $ do
        qualifiedRepo <- sample genQualifiedRepo
        let basicDependencies =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierRepo qualifiedRepo (Just $ DependencyName "dep-1")) Nothing,
                  BasicDependency JavaScript (DependencyIdentifierRepo qualifiedRepo (Just $ DependencyName "dep-1")) Nothing
                ]
            result = removeDuplicates basicDependencies
            expected =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierRepo qualifiedRepo (Just $ DependencyName "dep-1")) Nothing
                ]

        result `shouldBe` expected

      it "differing registries are not considered dups" $ do
        let basicDependencies =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing,
                  BasicDependency Haskell (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing
                ]
            result = removeDuplicates basicDependencies

        result `shouldBe` basicDependencies

      it "differing lanugage but same registry are considered dups" $ do
        let basicDependencies =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing,
                  BasicDependency TypeScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing
                ]
            result = removeDuplicates basicDependencies
            expected =
              V.fromList
                [ BasicDependency JavaScript (DependencyIdentifierNamed $ DependencyName "dep-1") Nothing
                ]

        result `shouldBe` expected
