{-# LANGUAGE DataKinds #-}

module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Version.MavenLatestVersion
  ( MavenLatestVersion (..),
    fetchLatestVersion,
    getUrlBaseWithVersion,
  )
where

import Common.Model.Ecosystem.ProgrammingLanguage
import Control.Monad.Except (throwError)
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenDetermineLatestRelease
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Version.MavenScalaLatestVersionText
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Network.HTTP.Req
import Text.HTML.TagSoup qualified as T

fetchLatestVersion :: ProgrammingLanguage -> Text -> Text -> ExceptT FetchDependencyRegistryError IO (Maybe MavenLatestVersion)
fetchLatestVersion programmingLanguage namespace name = do
  maybeScalaVersionText <-
    case programmingLanguage of
      Scala -> fetchScalaLatestVersionText namespace name
      Java -> pure Nothing
      Kotlin -> pure Nothing
      other -> throwError $ FDRRegistryFetchExceptional $ "Unexpected programming language to fetchLatestVersion: " <> show other

  -- fetch maven version
  let url = getUrlBaseWithVersion namespace name maybeScalaVersionText /: ""

  -- maven sometimes will 404 for no apparent reason, so we retry to avoid that problem
  maybeResult <- ExceptT $ fetchUrlRetry404 2 url
  case maybeResult of
    Nothing -> pure Nothing
    Just result -> do
      maybeReleaseVersion <- hoistEither $ first FDRFailureToParseResult $ getLatestVersion $ decodeUtf8 result
      pure $ flip MavenLatestVersion maybeScalaVersionText <$> maybeReleaseVersion

data MavenLatestVersion = MavenLatestVersion
  { _version :: Text,
    _maybeScalaVersion :: Maybe Text
  }
  deriving stock (Show, Generic)

getUrlBaseWithVersion :: Text -> Text -> Maybe Text -> Url 'Https
getUrlBaseWithVersion namespace name maybeScalaVersion =
  let namespaceAsUrlPattern = Text.replace "." "/" namespace
      -- https://repo1.maven.org/maven2/org/hibernate/hibernate-core/
      url = https "repo.maven.apache.org" /: "maven2" /: namespaceAsUrlPattern
   in case maybeScalaVersion of
        Nothing -> url /: name
        Just scalaVersion -> url /: (name <> "_" <> scalaVersion)

{-
<pre id="contents">
  <a href="../">../</a>
  <a href="1.5/" title="1.5/">1.5/</a>                                              2005-09-20 05:47         -
  <a href="1.5.1/" title="1.5.1/">1.5.1/</a>                                            2005-09-20 05:47         -
  <a href="1.5.5/" title="1.5.5/">1.5.5/</a>                                            2005-09-20 05:47         -
  <a href="2.0/" title="2.0/">2.0/</a>                                              2005-09-20 05:47         -
  <a href="2.1/" title="2.1/">2.1/</a>                                              2005-09-20 05:47         -
  <a href="2.2/" title="2.2/">2.2/</a>                                                             -         -
  <a href="maven-metadata.xml" title="maven-metadata.xml">maven-metadata.xml</a>                                2005-09-20 05:47       320
  <a href="maven-metadata.xml.md5" title="maven-metadata.xml.md5">maven-metadata.xml.md5</a>                            2005-09-20 05:47        69
  <a href="maven-metadata.xml.sha1" title="maven-metadata.xml.sha1">maven-metadata.xml.sha1</a>                           2005-09-20 05:47       124
</pre>
-}

getLatestVersion :: Text -> Either Text (Maybe Text)
getLatestVersion html = do
  let tags = T.parseTags html
  anchorVersions <- maybeToRight ("Unexpected missing href: " <> html) $ traverse (fmap (T.fromAttrib "href") . headMaybe) $ T.partitions (T.isTagOpenName "a") tags
  let filteredNonVersions = V.filter nonVersion $ stripSuffix "/" <$> V.fromList anchorVersions
      maybeNvVersions = NV.fromVector filteredNonVersions
  let result = for maybeNvVersions determineLatestRelease
  pure $
    join result

nonVersion :: Text -> Bool
nonVersion input
  | input == ".." = False
  | Text.isInfixOf "maven-metadata.xml" input = False
  | otherwise = True
