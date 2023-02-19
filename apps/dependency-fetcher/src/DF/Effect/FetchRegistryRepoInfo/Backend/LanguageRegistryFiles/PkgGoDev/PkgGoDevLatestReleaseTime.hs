{-# LANGUAGE DataKinds #-}

module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.PkgGoDev.PkgGoDevLatestReleaseTime
  ( PkgGoDevLatestReleaseTime (..),
    fetchLatestReleaseTime,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Parsing.Megaparsec
import Control.Monad.Except (throwError)
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson
import Data.Vector qualified as V
import Data.Versions qualified as VR
import Network.HTTP.Req
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

newtype PkgGoDevLatestReleaseTime = PkgGoDevLatestReleaseTime
  { _time :: Maybe UTCTime
  }
  deriving stock (Show, Generic)

fetchLatestReleaseTime :: DependencyName -> ExceptT FetchDependencyRegistryError IO PkgGoDevLatestReleaseTime
fetchLatestReleaseTime dependencyName = do
  maybeLatestVersion <- fetchLatestVersion dependencyName
  case maybeLatestVersion of
    Nothing -> pure $ PkgGoDevLatestReleaseTime Nothing
    Just latestVersion -> PkgGoDevLatestReleaseTime . Just <$> fetchVersionReleaseTime dependencyName latestVersion

-- https://proxy.golang.org/github.com/spf13/cobra/@v/list
fetchLatestVersion :: DependencyName -> ExceptT FetchDependencyRegistryError IO (Maybe Text)
fetchLatestVersion dependencyName = do
  let dependencyNameText = toLower $ dependencyName ^. #_ntText
      fullUrl = https "proxy.golang.org" /: dependencyNameText /: "@v" /: "list"
  maybeVersionsBS <- ExceptT $ fetchUrl fullUrl
  case maybeVersionsBS of
    Nothing -> pure Nothing
    Just versionsBS ->
      case M.parse parserVersions "Golang Versions" (decodeUtf8 versionsBS) of
        Right latestVersions -> pure $ VR.prettySemVer <$> safeMaximumV latestVersions
        Left e -> throwError (FDRFailureToParseResult $ "Unable to parse result for name" <> show dependencyNameText <> " error: " <> show (M.errorBundlePretty e))

{-
v1.0.0
v0.0.1
v1.1.1
v0.0.5
v1.1.3
v1.1.2
-}
parserVersions :: MParser (V.Vector VR.SemVer)
parserVersions = V.fromList <$> M.many parserVersion

-- semver' already lexes around whitespace, so don't need that
parserVersion :: MParser VR.SemVer
parserVersion = M.char 'v' *> VR.semver'

-- https://proxy.golang.org/github.com/spf13/cobra/@v/v1.1.3.info
fetchVersionReleaseTime :: DependencyName -> Text -> ExceptT FetchDependencyRegistryError IO UTCTime
fetchVersionReleaseTime dependencyName version = do
  let dependencyNameText = toLower $ dependencyName ^. #_ntText
      fullUrl = https "proxy.golang.org" /: dependencyNameText /: "@v" /: ("v" <> version <> ".info")
  maybePkgReleaseTime <- ExceptT $ fetchJSON @PkgGoDevReleaseTime fullUrl
  case maybePkgReleaseTime of
    Nothing -> throwError $ FDRRegistryDataInconsistent $ "Inconsisent for name:" <> show dependencyNameText
    Just pkgReleaseTime -> pure $ pkgReleaseTime ^. #_releaseDateTime

newtype PkgGoDevReleaseTime = PkgGoDevReleaseTime
  { _releaseDateTime :: UTCTime
  }
  deriving stock (Show, Generic)

{-
{
"Version": "v1.1.3",
"Time": "2021-02-10T19:41:09Z"
}
-}

instance FromJSON PkgGoDevReleaseTime where
  parseJSON =
    withObject "PkgGoDevReleaseTime" $ \v -> do
      publishedTime <- v .: "Time"
      pure $ PkgGoDevReleaseTime publishedTime
