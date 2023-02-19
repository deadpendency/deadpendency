module Effect.DetermineDependencies.Backend.LanguageFiles.Python.RequirementsTxtSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Python.RequirementsTxt
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

{-
#
####### example-requirements.txt #######
#
###### Requirements without Version Specifiers ######
nose
nose-cov
beautifulsoup4
#
###### Requirements with Version Specifiers ######
#   See https://www.python.org/dev/peps/pep-0440/#version-specifiers
docopt == 0.6.1             # Version Matching. Must be version 0.6.1
keyring >= 4.1.1            # Minimum version 4.1.1
coverage != 3.5             # Version Exclusion. Anything except version 3.5
Mopidy-Dirble ~= 1.1        # Compatible release. Same as >= 1.1, == 1.*
#
###### Refer to other requirements files ######
-r other-requirements.txt
#
#
###### A particular file ######
./downloads/numpy-1.9.2-cp34-none-win32.whl
http://wxpython.org/Phoenix/snapshot-builds/wxPython_Phoenix-3.0.3.dev1820+49a8884-cp34-none-win_amd64.whl
#
###### Additional Requirements without Version Specifiers ######
#   Same as 1st section, just here to show that you can put things in any order.
rejected
green
# Additional tests
SomeProject[foo, bar]
SomeProject2; sys_platform == 'win32'
SomeProject3 @ file:///somewhere/blah
python
#
-}

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a RequirementsTxt" $ do
      it "determines happy day dependencies" $ do
        -- generates hlint warning - waiting on https://github.com/ndmitchell/hlint/issues/844
        let inputRequirements =
              [r|
docopt == 0.6.1             # Version Matching. Must be version 0.6.1
keyring >= 4.1.1            # Minimum version 4.1.1

# this is a comment

coverage != 3.5             # Version Exclusion. Anything except version 3.5

Mopidy-Dirble ~= 1.1
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "docopt") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "keyring") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "coverage") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "Mopidy-Dirble") Nothing
                  ]

        result `shouldBe` expected

      it "python is ignored as a dependency" $ do
        let inputRequirements =
              [r|
python
keyring
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.singleton (BasicDependency Python (DependencyIdentifierNamed $ DependencyName "keyring") Nothing)

        result `shouldBe` expected

      it "loads deps with no initial newline" $ do
        let inputRequirements =
              [r|nose
django
confuse
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "nose") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "django") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "confuse") Nothing
                  ]

        result `shouldBe` expected

      it "extra dependencies are loaded" $ do
        let inputRequirements =
              [r|
./downloads/numpy-1.9.2-cp34-none-win32.whl
keyring
./downloads/numpy-1.9.2-cp34-none-win32.whl
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.singleton (BasicDependency Python (DependencyIdentifierNamed $ DependencyName "keyring") Nothing)

        result `shouldBe` expected

      it "dependency extras are ignored" $ do
        let inputRequirements =
              [r|
SomeProject[foo, bar]

cachecontrol[filecache]==0.12.10 \
    --hash=sha256:b0d43d8f71948ef5ebdee5fe236b86c6ffc7799370453dccb0e894c20dfa487c \
    --hash=sha256:d8aca75b82eec92d84b5d6eb8c8f66ea16f09d2adb09dbca27fe2d5fc8d3732d
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "SomeProject") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "cachecontrol") Nothing
                  ]

        result `shouldBe` expected

      it "repo dependencies are loaded" $ do
        let inputRequirements =
              [r|
git+git://github.com/blah/package-repo-one@41b95ec#egg=package-one
git+https://github.com/blah/package-repo-two@master#egg=package-two
git+ssh://github.com/blah/package-repo-three@0.1#egg=package-three
git+ssh://github.com/blah/package-repo-four#egg=package-four
git+git://github.com/blah/package-repo-five
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "blah") (RepoName "package-repo-one")) (Just $ DependencyName "package-one")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "blah") (RepoName "package-repo-two")) (Just $ DependencyName "package-two")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "blah") (RepoName "package-repo-three")) (Just $ DependencyName "package-three")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "blah") (RepoName "package-repo-four")) (Just $ DependencyName "package-four")) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitHub (RepoOwner "blah") (RepoName "package-repo-five")) Nothing) Nothing
                  ]

        result `shouldBe` expected

      it "other repo hosts are loaded" $ do
        let inputRequirements =
              [r|
git+git://bitbucket.org/blah/package-repo-one
git+https://gitlab.com/blah/package-repo-two
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo Bitbucket (RepoOwner "blah") (RepoName "package-repo-one")) Nothing) Nothing,
                    BasicDependency Python (DependencyIdentifierRepo (QualifiedRepo GitLab (RepoOwner "blah") (RepoName "package-repo-two")) Nothing) Nothing
                  ]

        result `shouldBe` expected

      it "includes are ignored" $ do
        let inputRequirements =
              [r|-r ../requirements-dev.in
django
confuse

-r other-requirements.txt

    # via -r requirements-dev.in
|]
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Python (DependencyIdentifierNamed $ DependencyName "django") Nothing,
                    BasicDependency Python (DependencyIdentifierNamed $ DependencyName "confuse") Nothing
                  ]

        result `shouldBe` expected

      it "handles no dependencies" $ do
        let inputRequirements = ""
            input = RequirementsTxtInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected
