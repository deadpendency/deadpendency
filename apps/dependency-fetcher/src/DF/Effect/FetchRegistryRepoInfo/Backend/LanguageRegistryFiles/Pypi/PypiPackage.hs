module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.PypiPackage
  ( PypiPackage (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Aeson
import Data.Vector qualified as V

newtype PypiPackage = PypiPackage
  { _result :: DependencyRegistryInfo
  }
  deriving stock (Show)

instance FromJSON PypiPackage where
  parseJSON =
    withObject "PypiPackage" $ \v -> do
      info <- v .: "info"
      maybeProjectUrls <- info .:? "project_urls"
      maybeRepo <-
        join
          <$> for
            maybeProjectUrls
            \projectUrls -> do
              maybeSourceUrl <- optional $ projectUrls .:~ "Source"
              maybeSourceRepositoryUrl <- optional $ projectUrls .:~ "Source Code"
              maybeRepositoryUrl <- optional $ projectUrls .:~ "Repository"
              maybeCodeUrl <- optional $ projectUrls .:~ "Code"
              maybeIssueTrackerUrl <- optional $ projectUrls .:~ "Issue Tracker"
              maybeHomepageUrl <- optional $ projectUrls .:~ "Homepage"
              let sourceRepos =
                    V.fromList [maybeSourceUrl, maybeSourceRepositoryUrl, maybeRepositoryUrl, maybeCodeUrl]
                      <&> \mRepoAsText -> mRepoAsText >>= mParseMaybe parserRepo
                  projectRepos =
                    V.fromList [maybeIssueTrackerUrl, maybeHomepageUrl]
                      <&> \mRepoAsText -> mRepoAsText >>= mParseMaybe parserQualifiedRepo
              pure $
                selectFinalRepo sourceRepos projectRepos

      latestVersion <- info .: "version"
      releases <- v .: "releases"
      latestReleaseUploads <- releases .: latestVersion
      latestReleaseDateTime <-
        case latestReleaseUploads of
          [] -> pure Nothing
          (l : _) -> Just <$> l .: "upload_time_iso_8601"

      pure $
        PypiPackage $
          DependencyRegistryInfo
            Pypi
            maybeRepo
            RASAlive
            latestReleaseDateTime
