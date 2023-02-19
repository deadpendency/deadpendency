{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module CommonTest.Gen.Model.Git where

import Common.Model.Git.GitFileMatch
import Common.Model.Git.GitPath
import Common.Model.Git.GitRef
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import CommonTest.Gen.General
import Hedgehog
import Hedgehog.Gen qualified as Gen

genGitRef :: Gen GitRef
genGitRef = GitRef <$> genAlphaText

genGitSha :: Gen GitSha
genGitSha = GitSha <$> genAlphaText

genGitPath :: Gen GitPath
genGitPath = GitPath <$> genAlphaText

genGitFileMatch :: Gen GitFileMatch
genGitFileMatch = GitFileMatch <$> genAlphaText

genRepo :: Gen Repo
genRepo =
  Gen.choice
    [ RepoQR <$> genQualifiedRepo,
      RepoUnknown <$> genURI
    ]

genQualifiedRepo :: Gen QualifiedRepo
genQualifiedRepo =
  QualifiedRepo
    <$> genRepoHost
    <*> genRepoOwner
    <*> genRepoName

genRepoHost :: Gen RepoHost
genRepoHost = Gen.enumBounded

genRepoOwner :: Gen RepoOwner
genRepoOwner = RepoOwner <$> genAlphaText

genRepoName :: Gen RepoName
genRepoName = RepoName <$> genAlphaText
