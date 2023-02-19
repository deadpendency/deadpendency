module Common.Effect.GitHub.SearchRepoFiles.Carrier.SearchRepoFilesGitHubC
  ( SearchRepoFilesGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.InstallationAuth.InstallationAuth (InstallationAuth (..), existingInstallationAuth)
import Common.Effect.GitHub.SearchRepoFiles.Backend.Model.GetAllRepoFile
import Common.Effect.GitHub.SearchRepoFiles.Backend.SearchRepoFilesBackend
import Common.Effect.GitHub.SearchRepoFiles.Model.SearchRepoFilesResult
import Common.Effect.GitHub.SearchRepoFiles.SearchRepoFiles (SearchRepoFiles (..))
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)
import Data.Vector qualified as V
import System.FilePath.Glob qualified as G

newtype SearchRepoFilesGitHubIOC m a = SearchRepoFilesGitHubIOC {runSearchRepoFilesGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has InstallationAuth sig m,
    Has (Throw CommonError) sig m
  ) =>
  Algebra (SearchRepoFiles :+: sig) (SearchRepoFilesGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (RepoFilesSearch request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Search Repo Files") (AppEventAdditional request)
      ghAuth <- existingInstallationAuth
      let qualifiedRepo = request ^. #_qualifiedRepo
          commitSha = request ^. #_commitSha
      eitherAllFiles <- liftIO $ githubGetAllRepoFiles ghAuth qualifiedRepo commitSha
      allFiles <- liftEither eitherAllFiles
      let rawTextFileMatch = request ^. (#_filesMatch . #_ntText)
      let toMatch = G.compile $ unpack rawTextFileMatch
      let toFetchFiles = V.filter (matches toMatch) allFiles
          filePathsToRequest = toFetchFiles <&> \f -> f ^. #_fullPath

      let result =
            SearchRepoFilesResult
              { _repoFilePaths = filePathsToRequest
              }
      emitAppEventInfoA (AppEventMessage "Finished: Search Repo Files") (AppEventAdditional result)
      SearchRepoFilesGitHubIOC $ pure (ctx $> result)
    (R other) -> SearchRepoFilesGitHubIOC $ alg (runSearchRepoFilesGitHubIOC . hdl) other ctx

matches :: G.Pattern -> GetAllRepoFile -> Bool
matches pat getAllRepoFile =
  let fileNameString = unpack $ getAllRepoFile ^. #_fileName
   in G.match pat fileNameString
