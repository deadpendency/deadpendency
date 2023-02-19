module Common.Effect.GitHub.SearchRepoDirectoryFiles.Carrier.SearchRepoDirectoryFilesGitHubC
  ( SearchRepoDirectoryFilesGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.InstallationAuth.InstallationAuth (InstallationAuth (..), existingInstallationAuth)
import Common.Effect.GitHub.SearchRepoDirectoryFiles.Backend.Model.GetDirectoryRepoFile
import Common.Effect.GitHub.SearchRepoDirectoryFiles.Backend.SearchRepoDirectoryFilesBackend
import Common.Effect.GitHub.SearchRepoDirectoryFiles.Model.SearchRepoDirectoryFilesResult
import Common.Effect.GitHub.SearchRepoDirectoryFiles.SearchRepoDirectoryFiles (SearchRepoDirectoryFiles (..))
import Common.Model.Error.CommonError
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)
import Data.Vector qualified as V
import System.FilePath.Glob qualified as G

newtype SearchRepoDirectoryFilesGitHubIOC m a = SearchRepoDirectoryFilesGitHubIOC {runSearchRepoDirectoryFilesGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  ( Algebra sig m,
    MonadIO m,
    Has AppEventEmit sig m,
    Has InstallationAuth sig m,
    Has (Throw CommonError) sig m
  ) =>
  Algebra (SearchRepoDirectoryFiles :+: sig) (SearchRepoDirectoryFilesGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (RepoDirectoryFilesSearch request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Search Repo Files") (AppEventAdditional request)
      ghAuth <- existingInstallationAuth
      let qualifiedRepo = request ^. #_qualifiedRepo
          commitSha = request ^. #_commitSha
          directoryPath = request ^. #_directoryPath
      eitherAllFiles <- liftIO $ githubGetDirectoryRepoFiles ghAuth qualifiedRepo commitSha directoryPath
      allFiles <- liftEither eitherAllFiles
      let rawTextFileMatch = request ^. (#_filesMatch . #_ntText)
      let toMatch = G.compile $ unpack rawTextFileMatch
      let toFetchFiles = V.filter (matches toMatch) allFiles
          filePathsToRequest = toFetchFiles <&> \f -> f ^. #_fullPath

      let result =
            SearchRepoDirectoryFilesResult
              { _repoFilePaths = filePathsToRequest
              }
      emitAppEventInfoA (AppEventMessage "Finished: Search Repo Files") (AppEventAdditional result)
      SearchRepoDirectoryFilesGitHubIOC $ pure (ctx $> result)
    (R other) -> SearchRepoDirectoryFilesGitHubIOC $ alg (runSearchRepoDirectoryFilesGitHubIOC . hdl) other ctx

matches :: G.Pattern -> GetDirectoryRepoFile -> Bool
matches pat getAllRepoFile =
  let fileNameString = unpack $ getAllRepoFile ^. #_fileName
   in G.match pat fileNameString
