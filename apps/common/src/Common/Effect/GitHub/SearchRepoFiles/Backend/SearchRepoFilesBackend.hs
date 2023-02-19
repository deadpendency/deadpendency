{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Effect.GitHub.SearchRepoFiles.Backend.SearchRepoFilesBackend
  ( githubGetAllRepoFiles,
  )
where

import Common.Effect.GitHub.SearchRepoFiles.Backend.Model.GetAllRepoFile
import Common.GitHub.GraphQLCommon
import Common.Model.Error.CommonError
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.Auth.GHInstallationAuth
import Data.Morpheus.Client
import Data.Vector qualified as V

declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    query GetAllRepoFiles ($repoOwner: String!, $repoName: String!, $commit: GitObjectID!) {
      repository(name: $repoName, owner: $repoOwner) {
        object(oid: $commit) {
          __typename
          ... on Commit {
            tree {
              entries {
                name
                type
                nObject: object {
                  __typename
                  ... on Tree {
                    nEntries: entries {
                      nName: name
                      nType: type
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  |]

githubGetAllRepoFiles :: GHInstallationAuth -> QualifiedRepo -> GitSha -> IO (Either CommonError (V.Vector GetAllRepoFile))
githubGetAllRepoFiles gitAuth qualifiedRepo commitSha = do
  eitherRawCheckRun <- mutGetAllRepoFiles authToken args
  pure $ asCommonMissingError eitherRawCheckRun >>= mapRunOutput
  where
    args = produceRunInput qualifiedRepo commitSha
    authToken = gitAuth ^. #_token

mutGetAllRepoFiles :: Text -> GetAllRepoFilesArgs -> IO (Either (FetchError GetAllRepoFiles) GetAllRepoFiles)
mutGetAllRepoFiles = fetch . executeGraphQL

produceRunInput :: QualifiedRepo -> GitSha -> GetAllRepoFilesArgs
produceRunInput qualifiedRepo commitSha =
  GetAllRepoFilesArgs
    { repoOwner = repoOwner',
      repoName = repoName',
      commit = commitShaObject'
    }
  where
    repoOwner' = qualifiedRepo ^. (#_repoOwner . #_ntText)
    repoName' = qualifiedRepo ^. (#_repoName . #_ntText)
    commitShaObject' = GitObjectID $ commitSha ^. #_ntText

mapRunOutput :: GetAllRepoFiles -> Either CommonError (V.Vector GetAllRepoFile)
mapRunOutput rcs = first GitHubResponseDecodeError $
  do
    repository' <- maybeToRight "Repository missing from fetch repo stats" $ repository rcs
    repositoryObject' <- maybeToRight "Repository Commit Missing" $ object repository'
    repositoryObjectCommit' <- getCommitFromObject repositoryObject'
    let tree' = tree repositoryObjectCommit'
    rootEntries' <- maybeToRight "Repository missing from fetch repo stats" $ V.fromList <$> entries tree'
    join <$> traverse produceFullPathEntries rootEntries'

getCommitFromObject :: GetAllRepoFilesRepositoryObject -> Either Text GetAllRepoFilesRepositoryObjectCommit
getCommitFromObject (GetAllRepoFilesRepositoryObjectVariantCommit commit) = Right commit
getCommitFromObject (GetAllRepoFilesRepositoryObjectVariantGitObject _) = Left "Unexpected git object"

-- will this blow up on an empty directory?
produceFullPathEntries :: GetAllRepoFilesRepositoryObjectTreeEntries -> Either Text (V.Vector GetAllRepoFile)
produceFullPathEntries (GetAllRepoFilesRepositoryObjectTreeEntries name' type' nObject)
  | type' == "blob" = Right $ V.singleton $ GetAllRepoFile name' (GitPath name')
  | type' == "tree" =
      case nObject >>= nEntries of
        Just subEntries -> Right $ fmap (produceRepoFile name' . nName) $ V.fromList subEntries
        Nothing -> Left $ "Unexpected Nothing in tree entries- name: " <> name'
  -- a commit indicates a submodule and repo files should not be loaded from submodules
  | type' == "commit" = Right V.empty
  | otherwise = Left $ "Unexpected non tree, blob or commit type: " <> type'

produceRepoFile :: Text -> Text -> GetAllRepoFile
produceRepoFile directory fileName =
  GetAllRepoFile
    { _fileName = fileName,
      _fullPath = GitPath $ directory <> cons '/' fileName
    }
