module RP.Effect.ReadConfig.Carrier.ReadConfigGitHubC
  ( ReadConfigGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.FetchRepoFiles.FetchRepoFiles
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesRequest
import Common.Model.Git.GitPath
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.Throw (Throw, liftEither)
import Data.Vector.NonEmpty qualified as NV
import RP.Effect.ReadConfig.Backend.ParseConfigBackend
import RP.Effect.ReadConfig.Model.ReadConfigError
import RP.Effect.ReadConfig.Model.ReadConfigResult
import RP.Effect.ReadConfig.ReadConfig (ReadConfig (..))

newtype ReadConfigGitHubIOC m a = ReadConfigGitHubIOC {runReadConfigGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Throw ReadConfigError) sig m,
    Has FetchRepoFiles sig m
  ) =>
  Algebra (ReadConfig :+: sig) (ReadConfigGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (ConfigRead request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Fetch Repo Config") (AppEventAdditional request)
      -- fetch config file
      let configFilePaths =
            NV.singleton (GitPath ".github/deadpendency.yaml")
              `NV.snoc` GitPath ".deadpendency/config.yaml"
          repoFilesRequest =
            RepoFilesRequest
              { _qualifiedRepo = request ^. #_gHQualifiedRepo,
                _commitSha = request ^. #_gitSha,
                _filePaths = configFilePaths
              }

      repoFilesResult <- repoFilesFetch repoFilesRequest
      let maybeRawConfig = repoFilesResult ^? (#_repoFiles . traversed . #_repoFile . _Just . #_fileContents)

      -- map text to config type
      maybeConfig <- liftEither $ traverse parseConfig maybeRawConfig

      let config' = fromMaybe defaultRepoConfig maybeConfig
          result =
            ReadConfigResult
              { _repoConfig = config'
              }
      emitAppEventInfoA (AppEventMessage "Finished: Fetch Repo Config") (AppEventAdditional result)
      ReadConfigGitHubIOC $ pure (ctx $> result)
    (R other) -> ReadConfigGitHubIOC $ alg (runReadConfigGitHubIOC . hdl) other ctx
