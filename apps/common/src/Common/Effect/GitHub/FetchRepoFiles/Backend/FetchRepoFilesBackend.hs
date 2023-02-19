{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Effect.GitHub.FetchRepoFiles.Backend.FetchRepoFilesBackend
  ( githubFetchRepoFile,
  )
where

import Common.Effect.GitHub.FetchRepoFiles.Backend.Model.InternalRepoFileRequest qualified as A
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesResult qualified as A
import Common.GitHub.GraphQLCommon
import Common.Model.Error.CommonError
import Common.Model.Git.GitPath
import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHRepoFile qualified as A
import Data.Morpheus.Client

-- https://github.com/morpheusgraphql/morpheus-graphql/issues/509 requires __typename for now
declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    query FetchRepoFile ($repoOwner: String!, $repoName: String!, $expression: String!) {
      repository(name: $repoName, owner: $repoOwner) {
        object(expression: $expression) {
          __typename
          ... on Blob {
            isTruncated
            text
          }
        }
      }
    }
  |]

githubFetchRepoFile :: GHInstallationAuth -> A.InternalRepoFileRequest -> IO (Either CommonError A.RepoFileResult)
githubFetchRepoFile gitAuth request' = do
  let args = produceFetchRepoFileInput request'
      filePath = request' ^. (#_filePath . #_ntText)
      authToken = gitAuth ^. #_token
  eitherRawFetchRepoFile <- qryFetchRepoFile authToken args
  pure $ asCommonError eitherRawFetchRepoFile >>= mapRunOutput filePath

qryFetchRepoFile :: Text -> FetchRepoFileArgs -> IO (Either (FetchError FetchRepoFile) FetchRepoFile)
qryFetchRepoFile = fetch . executeGraphQL

produceFetchRepoFileInput :: A.InternalRepoFileRequest -> FetchRepoFileArgs
produceFetchRepoFileInput request' =
  FetchRepoFileArgs
    { repoOwner = repoOwner,
      repoName = repoName,
      expression = expression
    }
  where
    repoOwner = request' ^. (#_qualifiedRepo . #_repoOwner . #_ntText)
    repoName = request' ^. (#_qualifiedRepo . #_repoName . #_ntText)
    gitSha = request' ^. (#_commitSha . #_ntText)
    filePath = stripPrefix "/" $ stripPrefix "./" $ request' ^. (#_filePath . #_ntText)
    expression = gitSha <> ":" <> filePath

mapRunOutput :: Text -> Maybe FetchRepoFile -> Either CommonError A.RepoFileResult
mapRunOutput filePath maybeFetchRepoFile =
  let gitPath = GitPath filePath
   in case maybeFetchRepoFile >>= repository of
        Nothing -> Left $ GitHubResponseDecodeError "Repository Missing"
        Just (FetchRepoFileRepository Nothing) -> Right $ A.RepoFileResult gitPath Nothing
        Just (FetchRepoFileRepository (Just (FetchRepoFileRepositoryObjectVariantGitObject _))) -> Left $ GitHubResponseDecodeError "Unexpected GitObject not Blob"
        Just (FetchRepoFileRepository (Just (FetchRepoFileRepositoryObjectVariantBlob repositoryObject'))) -> do
          let isTruncated' = isTruncated repositoryObject'
          when
            isTruncated'
            (Left $ GitHubResponseDecodeError $ "Response truncated: " <> filePath)
          let text' = fromMaybe "" (text repositoryObject') -- A file with no content can be represented by an empty string
          pure $
            A.RepoFileResult
              { _filePath = gitPath,
                _repoFile = Just $ A.GHRepoFile gitPath text'
              }
