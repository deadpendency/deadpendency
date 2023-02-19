module Effect.GitHub.SearchRepoFiles.SearchRepoFilesBackendSpec (spec) where

import Common.Effect.GitHub.SearchRepoFiles.Backend.Model.GetAllRepoFile
import Common.Effect.GitHub.SearchRepoFiles.Backend.SearchRepoFilesBackend (githubGetAllRepoFiles)
import Common.Model.Config.AppEnv
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import CommonTest.GitHub.TestGitHubAuth
import Data.Vector qualified as V
import Test.Hspec

spec :: Spec
spec =
  -- requires secrets which no longer exist in GCP
  xcontext "SearchRepoFilesBackendSpec" $
    beforeAll (reloadOrGenerateAuth Test) $
      it "gets all happy day files" $ \installAuth -> do
        let qualifiedRepo = QualifiedRepo GitHub (RepoOwner "deadpendency") (RepoName "deadpendency-test-repo")
            commitSha = GitSha "fb337664e7ce52603c49abbaab34aeac056956d6"

        resultFilePaths <- githubGetAllRepoFiles installAuth qualifiedRepo commitSha
        let expectedRepoFilePaths =
              V.fromList
                [ GetAllRepoFile ".gitignore" (GitPath ".gitignore"),
                  GetAllRepoFile "README.md" (GitPath "README.md"),
                  GetAllRepoFile "dev-environment" (GitPath "auto/dev-environment"),
                  GetAllRepoFile "docker-compose.yml" (GitPath "docker-compose.yml"),
                  GetAllRepoFile "package-lock.json" (GitPath "package-lock.json"),
                  GetAllRepoFile "package.json" (GitPath "package.json"),
                  GetAllRepoFile "favicon.ico" (GitPath "public/favicon.ico"),
                  GetAllRepoFile "index.html" (GitPath "public/index.html"),
                  GetAllRepoFile "logo192.png" (GitPath "public/logo192.png"),
                  GetAllRepoFile "logo512.png" (GitPath "public/logo512.png"),
                  GetAllRepoFile "manifest.json" (GitPath "public/manifest.json"),
                  GetAllRepoFile "robots.txt" (GitPath "public/robots.txt"),
                  GetAllRepoFile "App.css" (GitPath "src/App.css"),
                  GetAllRepoFile "App.js" (GitPath "src/App.js"),
                  GetAllRepoFile "App.test.js" (GitPath "src/App.test.js"),
                  GetAllRepoFile "index.css" (GitPath "src/index.css"),
                  GetAllRepoFile "index.js" (GitPath "src/index.js"),
                  GetAllRepoFile "logo.svg" (GitPath "src/logo.svg"),
                  GetAllRepoFile "serviceWorker.js" (GitPath "src/serviceWorker.js"),
                  GetAllRepoFile "setupTests.js" (GitPath "src/setupTests.js"),
                  GetAllRepoFile "test-contents-file.txt" (GitPath "test-contents-file.txt"),
                  GetAllRepoFile "yarn.lock" (GitPath "yarn.lock")
                ]

        resultFilePaths `shouldBe` Right expectedRepoFilePaths
