module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Version.MavenScalaLatestVersionText
  ( fetchScalaLatestVersionText,
  )
where

import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Text qualified as Text
import Data.Vector qualified as V
import Network.HTTP.Req
import Text.HTML.TagSoup qualified as T

-- if no Scala version is found we assume it is a Java dep
fetchScalaLatestVersionText :: Text -> Text -> ExceptT FetchDependencyRegistryError IO (Maybe Text)
fetchScalaLatestVersionText namespace name = do
  let namespaceAsUrlPattern = Text.replace "." "/" namespace
      -- https://repo.maven.apache.org/maven2/org/typelevel/
      url = https "repo.maven.apache.org" /: "maven2" /: namespaceAsUrlPattern /: ""
  -- maven sometimes will 404 for no apparent reason, so we retry to avoid that problem
  maybeResult <- ExceptT $ fetchUrlRetry404 2 url
  case maybeResult of
    Nothing -> pure Nothing
    Just result -> do
      hoistEither $ first FDRFailureToParseResult $ getLatestScalaVersion name $ decodeUtf8 result

{-
<pre id="contents">
<a href="cats-core_0.27/" title="cats-core_0.27/">cats-core_0.27/</a>                                                  -         -
<a href="cats-core_2.10/" title="cats-core_2.10/">cats-core_2.10/</a>                                                  -         -
<a href="cats-core_2.11/" title="cats-core_2.11/">cats-core_2.11/</a>                                                  -         -
<a href="cats-core_2.12/" title="cats-core_2.12/">cats-core_2.12/</a>                                                  -         -
<a href="cats-core_2.12.0-RC2/" title="cats-core_2.12.0-RC2/">cats-core_2.12.0-RC2/</a>                                            -         -
<a href="cats-core_2.13/" title="cats-core_2.13/">cats-core_2.13/</a>                                                  -         -                                              -         -
<a href="cats-core_3/" title="cats-core_3/">cats-core_3/</a>                                                     -         -
<a href="cats-core_3.0.0-M1/" title="cats-core_3.0.0-M1/">cats-core_3.0.0-M1/</a>
</pre>
-}

scalaVersions :: V.Vector Text
scalaVersions = V.fromList ["3", "2.13", "2.12", "2.11", "2.10"]

getLatestScalaVersion :: Text -> Text -> Either Text (Maybe Text)
getLatestScalaVersion dependencyNameText html = do
  let tags = T.parseTags html
  anchorVersions <- maybeToRight ("Unexpected missing href: " <> html) $ traverse (fmap (T.fromAttrib "href") . headMaybe) $ T.partitions (T.isTagOpenName "a") tags
  let filteredVersionMatches = V.filter (nameAndVersionMatch dependencyNameText) $ stripSuffix "/" <$> V.fromList anchorVersions
      maybeNewestVersion = safeLastV (sortV filteredVersionMatches)
  case maybeNewestVersion of
    Nothing -> pure Nothing
    Just newestVersionMatch -> do
      let underscoreVersionBit = Text.dropWhile (/= '_') newestVersionMatch
      -- tail is unsafe, so we do a bit of hoop jumping to make sure we avoid errors
      if Text.null underscoreVersionBit
        then Left $ "Unexpected matched version does not contain _ : " <> newestVersionMatch
        else Right $ Just $ Text.tail underscoreVersionBit

nameAndVersionMatch :: Text -> Text -> Bool
nameAndVersionMatch dependencyNameText input =
  let versionsWithName = scalaVersions <&> \v -> dependencyNameText <> "_" <> v
   in V.elem input versionsWithName
