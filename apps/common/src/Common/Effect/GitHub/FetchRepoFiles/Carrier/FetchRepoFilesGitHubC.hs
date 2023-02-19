module Common.Effect.GitHub.FetchRepoFiles.Carrier.FetchRepoFilesGitHubC
  ( FetchRepoFileGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.FetchRepoFiles.Backend.FetchRepoFilesBackend
import Common.Effect.GitHub.FetchRepoFiles.Backend.Model.InternalRepoFileRequest
import Common.Effect.GitHub.FetchRepoFiles.FetchRepoFiles (FetchRepoFiles (..))
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesResult
import Common.Effect.GitHub.InstallationAuth.InstallationAuth (InstallationAuth (..), existingInstallationAuth)
import Common.Model.Error.CommonError
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.Auth.GHInstallationAuth
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)
import Data.Vector.NonEmpty qualified as NV
import Streamly.Prelude qualified as S

newtype FetchRepoFileGitHubIOC m a = FetchRepoFileGitHubIOC {runFetchRepoFileGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has InstallationAuth sig m,
    Has (Throw CommonError) sig m
  ) =>
  Algebra (FetchRepoFiles :+: sig) (FetchRepoFileGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (RepoFilesFetch request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Fetch Repo Files") (AppEventAdditional request)
      ghAuth <- existingInstallationAuth
      let qualfiedRepo = request ^. #_qualifiedRepo
          gitSha = request ^. #_commitSha
          filePaths = request ^. #_filePaths
      eitherGetAllFilesResult <- liftIO $ getAllFiles ghAuth qualfiedRepo gitSha filePaths
      allFilesResult <- liftEither eitherGetAllFilesResult
      let result =
            RepoFilesResult
              { _repoFiles = allFilesResult
              }
      emitAppEventInfoA (AppEventMessage "Finished: Fetch Repo Files") (AppEventAdditional $ allFilesResult <&> \f -> f ^. #_filePath)

      FetchRepoFileGitHubIOC $ pure (ctx $> result)
    (R other) -> FetchRepoFileGitHubIOC $ alg (runFetchRepoFileGitHubIOC . hdl) other ctx

getAllFiles ::
  GHInstallationAuth ->
  QualifiedRepo ->
  GitSha ->
  NV.NonEmptyVector GitPath ->
  IO (Either CommonError (NV.NonEmptyVector RepoFileResult))
getAllFiles installationAuth qualifiedRepo gitSha gitPaths = do
  let execute = githubFetchRepoFile installationAuth . InternalRepoFileRequest qualifiedRepo gitSha
  result <- fmap sequenceA $ S.toList $ S.maxThreads 5 $ S.fromAhead $ S.mapM execute $ S.fromFoldable gitPaths
  pure $
    result
      >>= \deps ->
        case NV.fromList deps of
          Just nevDeps -> Right nevDeps
          _ -> Left UnexpectedEmptyDependenciesInStream
