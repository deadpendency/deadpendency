module GitHub.RepoStats.FetchRepoStatsSpec (spec) where

import Common.GitHub.RepoStats.FetchRepoStats
import Common.GitHub.RepoStats.Model.RepoStatsRequest
import Common.GitHub.RepoStats.Model.RepoStatsResult
import Common.Model.Config.AppEnv
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import CommonTest.GitHub.TestGitHubAuth
import Test.Hspec

spec :: Spec
spec =
  -- requires secrets which no longer exist in GCP
  xcontext "FetchRepoStatsSpec" $
    beforeAll (reloadOrGenerateAuth Test) $ do
      -- hmm this changes over time, better way?
      -- it "fetches happy day repo stats" $ \installAuth -> do
      --   let repoStatsRequest =
      --         RepoStatsRequest
      --           { _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "deadpendency") (RepoName "deadpendency-test-repo")
      --           }
      --   eitherRepoStatsResult <- fetchRepoStats installAuth repoStatsRequest
      --   -- this changes every time.. not sure of a nice way to ignore this from the expectation check, so we set it
      --   let expectedResult =
      --         RepoStatsResult
      --           { _dependencyRepoStats =
      --               Just $
      --                 DependencyRepoStats
      --                   { _twoYearlyCommitHistory =
      --                       V.fromList
      --                         [ DependencyRepoCommit {_commitDate = read "2021-02-22 03:55:53 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"},
      --                           DependencyRepoCommit {_commitDate = read "2020-08-19 02:57:13 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"},
      --                           DependencyRepoCommit {_commitDate = read "2020-03-27 02:33:10 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"},
      --                           DependencyRepoCommit {_commitDate = read "2020-03-24 05:27:32 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"},
      --                           DependencyRepoCommit {_commitDate = read "2020-02-25 22:29:34 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"},
      --                           DependencyRepoCommit {_commitDate = read "2020-02-18 00:54:04 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"},
      --                           DependencyRepoCommit {_commitDate = read "2020-02-18 00:33:47 UTC", _commitAuthorEmail = Just "alistair.burrowes@gmail.com"}
      --                         ],
      --                     _isArchived = False,
      --                     _isFork = False
      --                   }
      --           }

      --   eitherRepoStatsResult `shouldBe` Right expectedResult

      it "gracefully handles missing repos" $ \installAuth -> do
        let repoStatsRequest =
              RepoStatsRequest
                { _qualifiedRepo = QualifiedRepo GitHub (RepoOwner "does-not-actually-exist") (RepoName "for-sure-it-doesnt-roight")
                }
        eitherRepoStatsResult <- fetchRepoStats installAuth repoStatsRequest
        -- this changes every time.. not sure of a nice way to ignore this from the expectation check, so we set it
        let expectedResult =
              RepoStatsResult
                { _dependencyRepoStats = Nothing
                }

        eitherRepoStatsResult `shouldBe` Right expectedResult
