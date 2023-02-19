{-# LANGUAGE DataKinds #-}

module DD.Effect.DetermineDependencies.Carrier.DetermineDependenciesGitHubC
  ( DetermineDependenciesGitHubIOC (..),
  )
where

import Common.Effect.AppEventEmit.AppEventEmit
import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Effect.GitHub.FetchRepoFiles.FetchRepoFiles
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesRequest
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesResult
import Common.Effect.GitHub.SearchRepoDirectoryFiles.Model.SearchRepoDirectoryFilesRequest
import Common.Effect.GitHub.SearchRepoDirectoryFiles.SearchRepoDirectoryFiles
import Common.Effect.GitHub.SearchRepoFiles.Model.SearchRepoFilesRequest
import Common.Effect.GitHub.SearchRepoFiles.SearchRepoFiles
import Common.Effect.Util
import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.Basic.BasicRepoDependencies
import Common.Model.Dependency.File.DependenciesFileLoad
import Common.Model.Dependency.File.DependenciesFileLoadDetails
import Common.Model.Dependency.File.DependenciesFileType
import Common.Model.Dependency.Ignored.IgnoredRepoDependencies
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Error.CommonError
import Common.Model.Git.GitPath
import Common.Model.Git.GitSha
import Common.Model.Git.QualifiedRepo
import Common.Model.GitHub.GHRepoFile
import Common.Model.RepoConfig.FileLoadPlan
import Common.Model.RepoConfig.IgnoreDependenciesConfig (IgnoreDependenciesConfig)
import Common.Model.RepoConfig.RepoConfig
import Control.Algebra (Algebra (..), Has, (:+:) (..))
import Control.Effect.State (State)
import Control.Effect.Throw (Throw, liftEither, throwError)
import DD.Effect.DetermineDependencies.Backend.DependencyLanguageFilesBackend
import DD.Effect.DetermineDependencies.Backend.DetermineDependencyBackend
import DD.Effect.DetermineDependencies.Backend.Model.RawDepFileContent
import DD.Effect.DetermineDependencies.DetermineDependencies (DetermineDependencies (..))
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesResult
import Data.Map.Strict qualified as M
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV

newtype DetermineDependenciesGitHubIOC m a = DetermineDependenciesGitHubIOC {runDetermineDependenciesGitHubIOC :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance
  ( Algebra sig m,
    Has AppEventEmit sig m,
    Has (Throw DetermineDependenciesError) sig m,
    Has (Throw CommonError) sig m,
    Has FetchRepoFiles sig m,
    Has SearchRepoFiles sig m,
    Has SearchRepoDirectoryFiles sig m,
    Has (State (Maybe RepoConfig)) sig m
  ) =>
  Algebra (DetermineDependencies :+: sig) (DetermineDependenciesGitHubIOC m)
  where
  alg hdl sig ctx = case sig of
    (L (DetermineDependencies request)) -> do
      emitAppEventInfoA (AppEventMessage "Started: Determine dependencies") (AppEventAdditional request)

      repoConfig <- getRepoConfig
      let fileLoadPlan = repoConfig ^. #_fileLoadPlan
          programmingLanguages = request ^. #_programmingLanguages
          toLoadLanguages = factorLoadPlan fileLoadPlan programmingLanguages
          -- similar languages can produce the same dependency file load ie. java + kotlin
          dependenciesFileLoads = ordNubV $ V.concatMap determineDependencyLanguageFiles toLoadLanguages
          userFileLoads = request ^. #_additionalDependencyFiles
          qualifiedRepo = request ^. #_qualifiedRepo
          repoCommitSha = request ^. #_commitSha

      -- determine exact dep files to load
      depFileTypeLoads <- determineDepFiles qualifiedRepo repoCommitSha userFileLoads dependenciesFileLoads

      let filesToLoadCount = countDepFileLoads depFileTypeLoads

      when
        (filesToLoadCount > 100)
        (throwError $ TooManyDependencyFiles filesToLoadCount)

      -- fetch files
      rawDepFiles <- join <$> for depFileTypeLoads (loadDepTypesContent qualifiedRepo repoCommitSha)

      -- convert files into the actual dependencies
      initialDeps <- liftEither $ join <$> traverse loadFileDeps rawDepFiles

      let additionalDeps = request ^. #_additionalDependencies
          toIgnoreLanguageDeps = request ^. #_ignoreDependenciesConfig

      -- apply final de-dup, ignore and validity checks
      (ignoredRepoDependencies, nvFinalDeps) <- mungeFinalDeps additionalDeps toIgnoreLanguageDeps initialDeps

      let finalDepCount = NV.length nvFinalDeps

      when
        (finalDepCount > 500)
        (throwError $ TooManyDependencies finalDepCount)

      let result =
            DetermineDependenciesResult
              { _basicRepoDependencies = BasicRepoDependencies nvFinalDeps,
                _ignoredRepoDependencies = ignoredRepoDependencies
              }

      emitAppEventInfoA (AppEventMessage "Finished: Determine dependencies") (AppEventAdditional result)
      DetermineDependenciesGitHubIOC $ pure (ctx $> result)
    (R other) -> DetermineDependenciesGitHubIOC $ alg (runDetermineDependenciesGitHubIOC . hdl) other ctx

-- TYPES


{-
This is highly complex and untested.. It is probably the 'worst' code in the codebase.
It is something that has grown over time and towards the end of the project, where Deadpendency
was probably not going to survive. So it is a middle ground of effort vs maintainability.

Why is it so complex?

Firstly, the code in essence is quite simple:

1. Search for dependency files based on languages in use in the repository.
2. Fetch those files and parse them for dependencies.

There are 2 key requirements which escalate things:

1. The files we search and find ('system' files), we expect to never fail to load with a 404. If they do
  this can be considered a failure with the app logic and the code should go down the
  'oops we had an error' path.

  However, the files loaded can also include dependency files included by the user in their
  Deadpendency config ('user' files). If these files are missing, we consider that a user error and need to
  go down the 'hey you tried to load this dep file, but it doesn't exist path'.

2. We want to fetch all these files in parallel.

The tension is we fetch files with a simple FetchRepoFilesEffect which takes a [File], so if we
fetch these naively in parallel, we lose the information about which file is a system or a user file.
Thus we do not know how to fail in this case.

The ideal solution is a way to keep some tag for each file so we still know and can fetch them fully
in parallel. This is probably the most effort to implement as FetchRepoFilesEffect has no knowledge of
the wider application. Instead this code splits the files into each category and loads them each with
an independent call to FetchRepoFilesEffect, then collates the results.
-}
data DepFileTypeToLoad = DepFileTypeToLoad
  { _depFileType :: DependenciesFileType,
    -- 1. stuff we searched for and found to exist, just need to be fetched
    -- 2. stuff the user said to load specifically, is user error if not exist
    _paths :: These (NV.NonEmptyVector GitPath) (NV.NonEmptyVector GitPath)
  }
  deriving stock (Eq, Show, Generic)

-- DETERMINE

determineDepFiles ::
  ( Has SearchRepoFiles sig m,
    Has SearchRepoDirectoryFiles sig m
  ) =>
  QualifiedRepo ->
  GitSha ->
  V.Vector DependenciesFileLoad ->
  V.Vector DependenciesFileLoad ->
  m (V.Vector DepFileTypeToLoad)
determineDepFiles qualifiedRepo gitSha userFileLoads languageFileLoads = do
  let userGroupedDepFileLoads = groupDepFileLoads userFileLoads
      languageGroupedDepFileLoads = groupDepFileLoads languageFileLoads

  userDepFileTypeLoads <-
    concatMaybeV
      <$> for userGroupedDepFileLoads (uncurry (loadDepLoads qualifiedRepo gitSha True))

  languageDepFileTypeLoads <-
    concatMaybeV
      <$> for languageGroupedDepFileLoads (uncurry (loadDepLoads qualifiedRepo gitSha False))

  pure $ userDepFileTypeLoads V.++ languageDepFileTypeLoads

countDepFileLoads :: V.Vector DepFileTypeToLoad -> Int
countDepFileLoads loads =
  let (justSystem, justUser, bothSystemUserList) = partitionThese $ V.toList (fmap _paths loads)
      bothLength = sum $ fmap (\(system, user) -> NV.length system + NV.length user) bothSystemUserList
   in length justSystem + length justUser + bothLength

groupDepFileLoads :: V.Vector DependenciesFileLoad -> V.Vector (DependenciesFileType, NV.NonEmptyVector DependenciesFileLoadDetails)
groupDepFileLoads depFileLoads =
  let accLoadsToMap acc (DependenciesFileLoad loadType details) = M.insertWith (NV.++) loadType (NV.singleton details) acc
      resultAsMap = V.foldl' accLoadsToMap M.empty depFileLoads
   in V.fromList $ M.toList resultAsMap

loadDepLoads ::
  ( Has SearchRepoFiles sig m,
    Has SearchRepoDirectoryFiles sig m
  ) =>
  QualifiedRepo ->
  GitSha ->
  Bool ->
  DependenciesFileType ->
  NV.NonEmptyVector DependenciesFileLoadDetails ->
  m (Maybe DepFileTypeToLoad)
loadDepLoads qualifiedRepo gitSha isUserLoad depFileType fileLoadDetails = do
  nvLoads <- for fileLoadDetails (getFilesToLoad qualifiedRepo gitSha isUserLoad)
  let (allSystem, allUser) = NV.foldl' (\(system, user) (system', user') -> (system V.++ system', user V.++ user')) (V.empty, V.empty) nvLoads
      maybeNvSystem = NV.fromVector allSystem
      maybeNvUser = NV.fromVector allUser

  pure $
    DepFileTypeToLoad depFileType <$> maybesToThese maybeNvSystem maybeNvUser

-- FILE FETCH

getFilesToLoad ::
  ( Has SearchRepoFiles sig m,
    Has SearchRepoDirectoryFiles sig m
  ) =>
  QualifiedRepo ->
  GitSha ->
  Bool ->
  DependenciesFileLoadDetails ->
  m (V.Vector GitPath, V.Vector GitPath)
getFilesToLoad qualifiedRepo repoCommitSha isUserLoad loadDetails = do
  case loadDetails of
    DFLDSpecific filePath -> do
      let specificLoads = V.singleton (GitPath filePath)
      pure $
        if isUserLoad
          then (V.empty, specificLoads)
          else (specificLoads, V.empty)
    DFLDSearch fileMatch -> do
      let searchRequest =
            SearchRepoFilesRequest
              { _filesMatch = fileMatch,
                _qualifiedRepo = qualifiedRepo,
                _commitSha = repoCommitSha
              }
      searchResult <- repoFilesSearch searchRequest
      let resultPaths = searchResult ^. #_repoFilePaths
      pure
        (resultPaths, V.empty)
    DFLDDirectorySearch directory fileMatch -> do
      let searchRequest =
            SearchRepoDirectoryFilesRequest
              { _filesMatch = fileMatch,
                _directoryPath = directory,
                _qualifiedRepo = qualifiedRepo,
                _commitSha = repoCommitSha
              }
      searchResult <- repoDirectoryFilesSearch searchRequest
      let resultPaths = searchResult ^. #_repoFilePaths
      pure
        (resultPaths, V.empty)

loadDepTypesContent ::
  ( Has FetchRepoFiles sig m,
    Has (Throw DetermineDependenciesError) sig m
  ) =>
  QualifiedRepo ->
  GitSha ->
  DepFileTypeToLoad ->
  m (V.Vector RawDepFileContent)
loadDepTypesContent qualifiedRepo repoCommitSha depFileToLoad = do
  let depFileType = depFileToLoad ^. #_depFileType
      theseLoads = depFileToLoad ^. #_paths
  nvGHRepoFiles <-
    case theseLoads of
      (This systemLoads) -> loadSystemFilesContent qualifiedRepo repoCommitSha systemLoads
      (That userLoads) -> loadUserFilesContent qualifiedRepo repoCommitSha userLoads
      (These systemLoads userLoads) -> do
        systemFiles <- loadSystemFilesContent qualifiedRepo repoCommitSha systemLoads
        userFiles <- loadUserFilesContent qualifiedRepo repoCommitSha userLoads
        pure $
          systemFiles NV.++ userFiles

  let filteredSymLinks = NV.filter notSymLink nvGHRepoFiles

  pure $
    filteredSymLinks <&> RawDepFileContent depFileType

loadSystemFilesContent ::
  ( Has FetchRepoFiles sig m,
    Has (Throw DetermineDependenciesError) sig m
  ) =>
  QualifiedRepo ->
  GitSha ->
  NV.NonEmptyVector GitPath ->
  m (NV.NonEmptyVector GHRepoFile)
loadSystemFilesContent qualifiedRepo repoCommitSha filePaths = do
  let filesRequest =
        RepoFilesRequest
          { _qualifiedRepo = qualifiedRepo,
            _commitSha = repoCommitSha,
            _filePaths = filePaths
          }
  repoFilesResult <- repoFilesFetch filesRequest
  let fileResults = repoFilesResult ^. #_repoFiles
  results <- liftEither $ first (DependencyFilesMismatch . snd) $ traverse getResult fileResults
  pure $
    results <&> snd

loadUserFilesContent ::
  ( Has FetchRepoFiles sig m,
    Has (Throw DetermineDependenciesError) sig m
  ) =>
  QualifiedRepo ->
  GitSha ->
  NV.NonEmptyVector GitPath ->
  m (NV.NonEmptyVector GHRepoFile)
loadUserFilesContent qualifiedRepo repoCommitSha filePaths = do
  -- TODO: use these so we know we have gitpaths to avoid a possible error
  let filesRequest =
        RepoFilesRequest
          { _qualifiedRepo = qualifiedRepo,
            _commitSha = repoCommitSha,
            _filePaths = filePaths
          }
  repoFilesResult <- repoFilesFetch filesRequest
  let fileResults = repoFilesResult ^. #_repoFiles
  results <- liftEither $ first (UserSpecificedMissingFile . fst) $ traverse getResult fileResults
  pure $
    results <&> snd

-- FINAL MASSAGE

-- | Produce a final list of dependencies to be loaded
mungeFinalDeps ::
  (Has (Throw DetermineDependenciesError) sig m) =>
  V.Vector BasicDependency ->
  IgnoreDependenciesConfig ->
  V.Vector BasicDependency ->
  m (IgnoredRepoDependencies, NV.NonEmptyVector BasicDependency)
mungeFinalDeps additionalDeps ignoreDepConfig foundDeps = do
  let withAdditional = addAdditionalDeps additionalDeps foundDeps
      duplicatesIgnored = removeDuplicates withAdditional
  nvAllDeps <- maybeToErrorM NoDependenciesFound (NV.fromVector duplicatesIgnored)
  let (resultIgnoredDeps, maybeResultBasicRepoDeps) = ignoreDeps ignoreDepConfig nvAllDeps
  resultBasicRepoDeps <- maybeToErrorM AllDependenciesIgnored maybeResultBasicRepoDeps
  pure
    (resultIgnoredDeps, resultBasicRepoDeps)

-- UTILITY

-- this is an ugly way to detect a symlink eg. https://github.com/apache/airflow/blob/d8ae7df08168fd3ab92ed0d917f9b5dd34d1354d/provider_packages/pyproject.toml
-- will not match symlinks from the root to subdirs, but there is apparently no way to actually
-- detect a symlink using the graphql api
notSymLink :: GHRepoFile -> Bool
notSymLink (GHRepoFile _ fileContents) = not $ Text.isPrefixOf "../" fileContents

getResult :: RepoFileResult -> Either (GitPath, Text) (GitPath, GHRepoFile)
getResult (RepoFileResult filePath Nothing) = Left (filePath, "Unexpected missing file from search: " <> show filePath)
getResult (RepoFileResult filePath (Just repoFile)) = Right (filePath, repoFile)

factorLoadPlan :: FileLoadPlan -> V.Vector ProgrammingLanguage -> V.Vector ProgrammingLanguage
factorLoadPlan FileLoadEnabled input = input
factorLoadPlan FileLoadDisabled _ = V.empty
factorLoadPlan (FileLoadDisabledForLangs nvDisabledLangs) input = V.filter (`NV.notElem` nvDisabledLangs) input
