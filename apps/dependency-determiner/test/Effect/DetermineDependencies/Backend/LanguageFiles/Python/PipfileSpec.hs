module Effect.DetermineDependencies.Backend.LanguageFiles.Python.PipfileSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.DependencyType
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.Pipfile
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

{-
https://github.com/pypa/pipfile#examples-spec-v6
[packages]
requests = { extras = ['socks'] }
records = '>0.5.0'
django = { git = 'https://github.com/django/django.git', ref = '1.11.4', editable = true }
"e682b37" = {file = "https://github.com/divio/django-cms/archive/release/3.4.x.zip"}
"e1839a8" = {path = ".", editable = true}
pywinusb = { version = "*", os_name = "=='nt'", index="pypi"}

[dev-packages]
nose = '*'
unittest2 = {version = ">=1.0,<3.0", markers="python_version < '2.7.9' or (python_version >= '3.0' and python_version < '3.4')"}
-}

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a Pipfile" $ do
      it "determines a simple named dependencies" $ do
        let input =
              [r|
[packages]
e682b37 = "0.5.0"
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PipfileInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "e682b37") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "blah") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "strips quotes from named dependencies" $ do
        let input =
              [r|
[packages]
"records" = "0.5.0"
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PipfileInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "records") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "blah") (Just CoreDependency)
                    ]

        result `shouldBe` expected

      it "determines a core vs dev dependencies correctly" $ do
        let input =
              [r|
[packages]
records = ">0.5.0"
blah = "*"

[dev-packages]
other1 = ">0.5.0"
other2 = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PipfileInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "records") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "blah") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "other1") (Just DevDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "other2") (Just DevDependency)
                    ]

        result `shouldBe` expected

      it "determines qualified repo dependencies" $ do
        let input =
              [r|
[packages]
django = { git = "https://github.com/djangoowner/djangorepo.git", ref = "1.11.4", editable = true }
other = { file = "https://github.com/divio/django-cms/archive/release/3.4.x.zip" }
blah = "*"
|]
            result = fmap sortV (determineDependencies (GitPath "path") $ PipfileInput input)
            expected =
              Right $
                V.fromList $
                  sort
                    [ BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "djangoowner") (RepoName "djangorepo")) (Just $ DependencyName "django")) (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "other") (Just CoreDependency),
                      BasicDependency Python (DependencyIdentifierNamed $ DependencyName "blah") (Just CoreDependency)
                    ]

        result `shouldBe` expected

-- this is failing to encode the dependencies to toml format. Not sure why. Seems to be failing in tomland plumbing after producing the Maybe (HM.HashMap Text PackageDetails)
-- https://github.com/deadpendency/deadpendency-action/issues/177

-- import Hedgehog
-- import qualified Hedgehog.Range as Range
-- import CommonTest.Gen.Model.Dependency
-- import CommonTest.Gen.General
-- import Test.Hspec.Hedgehog

--       fit "roundtrips to from toml" $ hedgehog $ do
--         parsedPipfile <- forAll genParsedPipfile

--         annotateShow (Toml.encode parsedPipfileCodec parsedPipfile)

--         tripping parsedPipfile (Toml.encode parsedPipfileCodec) (Toml.decode parsedPipfileCodec)

-- genBasicDependencyOfType :: DependencyType -> Gen BasicDependency
-- genBasicDependencyOfType depType = do
--   depIdentifier <- genDependencyIdentifierOnlyNamed
--   pure $
--     BasicDependency Python depIdentifier (Just depType)

-- genParsedPipfile :: Gen ParsedPipfile
-- genParsedPipfile = ParsedPipfile
--   <$> genVector (Range.constant 0 10) (genBasicDependencyOfType CoreDependency)
--   <*> genVector (Range.constant 0 10) (genBasicDependencyOfType DevDependency)
