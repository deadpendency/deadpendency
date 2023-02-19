{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.GitHub.RepoStats.FetchRepoStats
  ( fetchRepoStats,
  )
where

import Common.GitHub.GraphQLCommon
import Common.GitHub.Model.GitHubError
import Common.GitHub.RepoStats.Model.RepoStatsRequest qualified as A
import Common.GitHub.RepoStats.Model.RepoStatsResult qualified as A
import Common.Model.Dependency.Repo.DependencyRepoCommit
import Common.Model.Dependency.Repo.DependencyRepoStats
import Common.Model.GitHub.Auth.GHInstallationAuth
import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Morpheus.Client
import Data.Vector qualified as V
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req
import Network.HTTP.Types.Header qualified as HC
import Network.HTTP.Types.Status qualified as HC

declareLocalTypesInline
  "../resource/minimal-github.graphql"
  [raw|
    query FetchRepoStats ($repoOwner: String!, $repoName: String!, $since: GitTimestamp!) {
      repository(name: $repoName, owner: $repoOwner) {
        isArchived
        isFork
        defaultBranchRef {
          target {
            __typename
            ... on Commit {
              history(first: 10, since: $since) {
                edges {
                  node {
                    pushedDate
                    committedDate
                    author {
                      authorEmail: email
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

fetchRepoStats :: GHInstallationAuth -> A.RepoStatsRequest -> IO (Either GitHubError A.RepoStatsResult)
fetchRepoStats gitAuth request' = do
  twoYearsAgoUTCTime <- getCurrentTime <&> addUTCTime (negate $ nominalDay * 730)
  let args = produceFetchRepoStatsInput twoYearsAgoUTCTime request'
      authToken = gitAuth ^. #_token
  eitherWrappedHttpException <- firstF getGitHubErrorFromHttpException $ try @_ @HttpException $ qryFetchRepoStats authToken args
  let eitherRawFetchRepoStats = asGitHubError =<< eitherWrappedHttpException
  pure $ eitherRawFetchRepoStats >>= mapRunOutput

getGitHubErrorFromHttpException :: HttpException -> GitHubError
getGitHubErrorFromHttpException f@(VanillaHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException response body))) =
  let statusCode = HC.statusCode $ HC.responseStatus response
      maybeRetryAfter = (readMaybe . decodeUtf8) =<< lookup HC.hRetryAfter (HC.responseHeaders response)
      bodyContainsAbuseOrSecondary = BS.isInfixOf "abuse" body || BS.isInfixOf "secondary rate limit" body
   in case (statusCode, maybeRetryAfter, bodyContainsAbuseOrSecondary) of
        (403, Just retryValue, True) -> GHEAbuseDetectedError retryValue
        _ -> GHEExceptionalError $ "Unexpected HTTP failure: " <> show f
getGitHubErrorFromHttpException unexpected = GHEExceptionalError $ "Unexpected HTTP failure: " <> show unexpected

asGitHubError :: (Show a) => Either (FetchError a) a -> Either GitHubError (Maybe a)
asGitHubError =
  \case
    Right a -> Right $ Just a
    Left (FetchErrorParseFailure parseFailure) -> Left $ GHEReponseParseError $ show parseFailure
    Left FetchErrorNoResult -> Left $ GHEExceptionalError "Unexpected no fetch result"
    Left (FetchErrorProducedErrors errors maybeA) ->
      if errorsContainsNotFound errors
        then Right Nothing
        else Left $ GHEExceptionalError $ "Errors: " <> show errors <> " Result: " <> show maybeA

qryFetchRepoStats :: Text -> FetchRepoStatsArgs -> IO (Either (FetchError FetchRepoStats) FetchRepoStats)
qryFetchRepoStats = fetch . executeGraphQL

produceFetchRepoStatsInput :: UTCTime -> A.RepoStatsRequest -> FetchRepoStatsArgs
produceFetchRepoStatsInput since request' =
  FetchRepoStatsArgs
    { repoOwner = repoOwner,
      repoName = repoName,
      since = GitTimestamp since
    }
  where
    repoOwner = request' ^. (#_qualifiedRepo . #_repoOwner . #_ntText)
    repoName = request' ^. (#_qualifiedRepo . #_repoName . #_ntText)

mapRunOutput :: Maybe FetchRepoStats -> Either GitHubError A.RepoStatsResult
mapRunOutput maybeFetchRepoStats =
  case maybeFetchRepoStats >>= repository of
    Nothing -> Right $ A.RepoStatsResult Nothing -- `repository: null` is the result if the repo is missing, which is not exceptional
    Just repository' -> first GHEReponseParseError $ do
      defaultBranchRef' <- maybeToRight "Default branch missing from fetch repo stats" $ defaultBranchRef repository'
      let target' = target defaultBranchRef'
      targetCommit' <- getCommitFromObject target'
      let history' = history targetCommit'

      listMaybeNodes' <- maybeToRight "Edges missing from connection for fetch repo stats" $ edges history'
      listMaybeCommits' <- node <<$>> maybesToEither "Some commits are null for fetch repo stats" listMaybeNodes'
      commits' <- V.fromList <$> maybesToEither "Some commits are null for fetch repo stats" listMaybeCommits'
      let outoutCommits' = fmap mapCommitToOutputCommit commits'
      let archived = isArchived repository'
          fork = isFork repository'
      pure
        A.RepoStatsResult
          { A._dependencyRepoStats =
              Just $
                DependencyRepoStats
                  { _twoYearlyCommitHistory = outoutCommits',
                    _isArchived = archived,
                    _isFork = fork
                  }
          }

getCommitFromObject :: FetchRepoStatsRepositoryDefaultBranchRefTarget -> Either Text FetchRepoStatsRepositoryDefaultBranchRefTargetCommit
getCommitFromObject (FetchRepoStatsRepositoryDefaultBranchRefTargetVariantCommit commit) = Right commit
getCommitFromObject (FetchRepoStatsRepositoryDefaultBranchRefTargetVariantGitObject _) = Left "Unexpected git object"

mapCommitToOutputCommit :: FetchRepoStatsRepositoryDefaultBranchRefTargetHistoryEdgesNode -> DependencyRepoCommit
mapCommitToOutputCommit repoCommit =
  let commitDateTime =
        _datetimeTime $
          case pushedDate repoCommit of
            Just pushedDate' -> pushedDate'
            Nothing -> committedDate repoCommit
      commitAuthorEmail = author repoCommit >>= authorEmail
   in DependencyRepoCommit
        { _commitDate = commitDateTime,
          _commitAuthorEmail = commitAuthorEmail
        }

-- pushed date can be missing. Perhaps when changes are made in the github editor.. or perhaps merge commits from PRs
-- in this case the commit date is the equivelant
