module Effect.GitHub.FetchRepoFiles.FetchRepoFilesBackendSpec (spec) where

import Common.Effect.GitHub.FetchRepoFiles.Backend.FetchRepoFilesBackend (githubFetchRepoFile)
import Common.Effect.GitHub.FetchRepoFiles.Backend.Model.InternalRepoFileRequest
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesResult
import Common.Model.Config.AppEnv
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Common.Model.GitHub.GHRepoFile
import CommonTest.GitHub.TestGitHubAuth
import Test.Hspec

spec :: Spec
spec =
  -- requires secrets which no longer exist in GCP
  xcontext "FetchRepoFilesBackendSpec" $
    beforeAll (reloadOrGenerateAuth Test) $ do
      context "FetchRepoFilesBackendSpec" $ do
        it "fetches a happy day file" $ \installAuth -> do
          let repoFileRequest =
                InternalRepoFileRequest
                  { _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "deadpendency") (RepoName "deadpendency-test-repo"),
                    _commitSha = GitSha "fb337664e7ce52603c49abbaab34aeac056956d6",
                    _filePath = GitPath "test-contents-file.txt"
                  }

          eitherRepoFilesResult <- githubFetchRepoFile installAuth repoFileRequest
          let expectedRepoFile =
                GHRepoFile
                  { _filePath = GitPath "test-contents-file.txt",
                    _fileContents = "1\n" -- seems that github will append a new line to the contents
                  }
              expectedRepoFilesResult =
                RepoFileResult (GitPath "test-contents-file.txt") (Just expectedRepoFile)

          eitherRepoFilesResult `shouldBe` Right expectedRepoFilesResult

        it "handles ./ in path" $ \installAuth -> do
          let repoFileRequest =
                InternalRepoFileRequest
                  { _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "deadpendency") (RepoName "deadpendency-test-repo"),
                    _commitSha = GitSha "fb337664e7ce52603c49abbaab34aeac056956d6",
                    _filePath = GitPath "./test-contents-file.txt"
                  }

          eitherRepoFilesResult <- githubFetchRepoFile installAuth repoFileRequest
          let expectedRepoFile =
                GHRepoFile
                  { _filePath = GitPath "./test-contents-file.txt",
                    _fileContents = "1\n" -- seems that github will append a new line to the contents
                  }
              expectedRepoFilesResult =
                RepoFileResult (GitPath "./test-contents-file.txt") (Just expectedRepoFile)

          eitherRepoFilesResult `shouldBe` Right expectedRepoFilesResult

        it "fetches a big happy day file" $ \installAuth -> do
          let repoFileRequest =
                InternalRepoFileRequest
                  { _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "deadpendency") (RepoName "deadpendency-test-repo"),
                    _commitSha = GitSha "fb337664e7ce52603c49abbaab34aeac056956d6",
                    _filePath = GitPath "package.json"
                  }

          eitherRepoFilesResult <- githubFetchRepoFile installAuth repoFileRequest
          let eitherIsFileFetched = eitherRepoFilesResult <&> \repoFilesResult -> isJust $ repoFilesResult ^. #_repoFile

          eitherIsFileFetched `shouldBe` Right True

        it "gracefully handles missing files" $ \installAuth -> do
          let repoFileRequest =
                InternalRepoFileRequest
                  { _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "deadpendency") (RepoName "deadpendency-test-repo"),
                    _commitSha = GitSha "fb337664e7ce52603c49abbaab34aeac056956d6",
                    _filePath = GitPath "not-exist.yaml"
                  }

              expectedRepoFilesResult =
                RepoFileResult (GitPath "not-exist.yaml") Nothing

          eitherRepoFilesResult <- githubFetchRepoFile installAuth repoFileRequest

          eitherRepoFilesResult `shouldBe` Right expectedRepoFilesResult
