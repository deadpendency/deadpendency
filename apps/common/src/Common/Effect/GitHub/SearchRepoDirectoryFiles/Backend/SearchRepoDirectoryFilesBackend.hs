{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Effect.GitHub.SearchRepoDirectoryFiles.Backend.SearchRepoDirectoryFilesBackend
  ( githubGetDirectoryRepoFiles,
  )
where

import Common.Effect.GitHub.SearchRepoDirectoryFiles.Backend.Model.GetDirectoryRepoFile
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
    query GetDirectoryRepoFiles ($repoOwner: String!, $repoName: String!, $expression: String!) {
      repository(name: $repoName, owner: $repoOwner) {
        object(expression: $expression) {
          __typename
          ... on Tree {
              entries {
                name
                type
              }
          }
        }
      }
    }
  |]

githubGetDirectoryRepoFiles :: GHInstallationAuth -> QualifiedRepo -> GitSha -> GitPath -> IO (Either CommonError (V.Vector GetDirectoryRepoFile))
githubGetDirectoryRepoFiles gitAuth qualifiedRepo commitSha directoryPath = do
  eitherRawCheckRun <- mutGetDirectoryRepoFiles authToken args
  pure $ asCommonMissingError eitherRawCheckRun >>= mapRunOutput directory
  where
    directory = stripPrefix "/" $ stripPrefix "./" $ directoryPath ^. #_ntText
    args = produceRunInput qualifiedRepo commitSha directory
    authToken = gitAuth ^. #_token

mutGetDirectoryRepoFiles :: Text -> GetDirectoryRepoFilesArgs -> IO (Either (FetchError GetDirectoryRepoFiles) GetDirectoryRepoFiles)
mutGetDirectoryRepoFiles = fetch . executeGraphQL

produceRunInput :: QualifiedRepo -> GitSha -> Text -> GetDirectoryRepoFilesArgs
produceRunInput qualifiedRepo commitSha directory =
  GetDirectoryRepoFilesArgs
    { repoOwner = repoOwner',
      repoName = repoName',
      expression = expression
    }
  where
    repoOwner' = qualifiedRepo ^. (#_repoOwner . #_ntText)
    repoName' = qualifiedRepo ^. (#_repoName . #_ntText)
    commitShaText = commitSha ^. #_ntText
    expression = commitShaText <> ":" <> directory

mapRunOutput :: Text -> GetDirectoryRepoFiles -> Either CommonError (V.Vector GetDirectoryRepoFile)
mapRunOutput directory rcs = first GitHubResponseDecodeError $
  do
    repository' <- maybeToRight "Repository missing from fetch dir files" $ repository rcs
    repositoryObjectCommit' <- maybeToRight "Repository Commit Missing" $ object repository'
    repositoryObjectCommitTree' <- getTreeFromObject repositoryObjectCommit'
    entries' <- V.fromList <<$>> maybeToRight "Directory entires missing" $ entries repositoryObjectCommitTree'
    join <$> traverse (produceFullPathEntries directory) entries'

getTreeFromObject :: GetDirectoryRepoFilesRepositoryObject -> Either Text GetDirectoryRepoFilesRepositoryObjectTree
getTreeFromObject (GetDirectoryRepoFilesRepositoryObjectVariantTree tree) = Right tree
getTreeFromObject (GetDirectoryRepoFilesRepositoryObjectVariantGitObject _) = Left "Unexpected git object"

-- -- will this blow up on an empty directory?
produceFullPathEntries :: Text -> GetDirectoryRepoFilesRepositoryObjectEntries -> Either Text (V.Vector GetDirectoryRepoFile)
produceFullPathEntries directory (GetDirectoryRepoFilesRepositoryObjectEntries name' type') =
  case type' of
    "blob" -> Right $ V.singleton $ produceRepoFile directory name'
    "tree" -> Right V.empty
    "commit" -> Right V.empty
    _ -> Left $ "Unexpected non tree, blob or commit type: " <> type'

produceRepoFile :: Text -> Text -> GetDirectoryRepoFile
produceRepoFile directory fileName =
  GetDirectoryRepoFile
    { _fileName = fileName,
      _fullPath = GitPath $ directory <> cons '/' fileName
    }
