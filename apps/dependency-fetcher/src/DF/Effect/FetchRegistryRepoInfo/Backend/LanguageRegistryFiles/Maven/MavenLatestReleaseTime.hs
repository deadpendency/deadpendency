module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenLatestReleaseTime
  ( MavenLatestReleaseTime (..),
    fetchLatestReleaseTime,
  )
where

import Common.Parsing.Megaparsec
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Version.MavenLatestVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Network.HTTP.Req
import Text.HTML.TagSoup qualified as T
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

fetchLatestReleaseTime :: Text -> Text -> MavenLatestVersion -> ExceptT FetchDependencyRegistryError IO MavenLatestReleaseTime
fetchLatestReleaseTime namespace name latestVersion = do
  let versionText = latestVersion ^. #_version
      maybeScalaVersion = latestVersion ^. #_maybeScalaVersion
      -- https://repo1.maven.org/maven2/org/hibernate/hibernate-core/5.4.9.Final/
      -- seems the trailing slash is actually important ie. https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.12.2
      url = getUrlBaseWithVersion namespace name maybeScalaVersion /: versionText /: mempty
  -- maven sometimes will 404 for no apparent reason, so we retry to avoid that problem
  maybeResult <- ExceptT $ fetchUrlRetry404 2 url
  result <- hoistEither $ maybeToRight (FDRRegistryDataInconsistent $ "Missing files pages for fetching latest release time: " <> show url) maybeResult
  releaseTime <- hoistEither $ first FDRFailureToParseResult $ getLatestReleaseTime name latestVersion result
  pure $ MavenLatestReleaseTime releaseTime

newtype MavenLatestReleaseTime = MavenLatestReleaseTime
  { _time :: UTCTime
  }
  deriving stock (Show, Generic)

{-
<!DOCTYPE html>
<html>
  <head>
  </head>

<body>
	<header>
		<h1>org/hibernate/hibernate-core/5.4.9.Final</h1>
	</header>
	<hr/>
	<main>
		<pre id="contents">
      <a href="hibernate-core-5.4.9.Final.jar.md5" title="hibernate-core-5.4.9.Final.jar.md5">hibernate-core-5.4.9.Final.jar.md5</a>                2019-11-14 16:19        32
      <a href="hibernate-core-5.4.9.Final.jar.sha1" title="hibernate-core-5.4.9.Final.jar.sha1">hibernate-core-5.4.9.Final.jar.sha1</a>               2019-11-14 16:19        40
      <a href="hibernate-core-5.4.9.Final.pom" title="hibernate-core-5.4.9.Final.pom">hibernate-core-5.4.9.Final.pom</a>                    2019-11-14 16:19      6135
		</pre>
	</main>
	<hr/>
</body>

</html>
-}

getLatestReleaseTime :: Text -> MavenLatestVersion -> ByteString -> Either Text UTCTime
getLatestReleaseTime name latestVersion htmlBS = do
  let versionText = latestVersion ^. #_version
      maybeScalaVersion = latestVersion ^. #_maybeScalaVersion
      -- gross hack because they renamed the old pom file not the new one
      -- https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.31/
      nameWithMysqlHack =
        if name == "mysql-connector-java"
          then "mysql-connector-j"
          else name
      depNameWithScala = maybe nameWithMysqlHack (\scalaVersion -> nameWithMysqlHack <> "_" <> scalaVersion) maybeScalaVersion
      jarFileName = depNameWithScala <> "-" <> versionText <> ".jar"
      pomFileName = depNameWithScala <> "-" <> versionText <> ".pom"
      jarLink = T.TagOpen "a" [("href", jarFileName)]
      pomLink = T.TagOpen "a" [("href", pomFileName)]
      jarAscLink = T.TagOpen "a" [("href", jarFileName <> ".asc")]
      pomAscLink = T.TagOpen "a" [("href", pomFileName <> ".asc")]
      tags = T.parseTags htmlBS
  statsTag <- maybeToRight "Unexpected missing version time as text" $ do
    let jarOrPomAsText = (!!? 3) $ take 4 $ dropWhile (\a -> (a T.~/= jarLink) && (a T.~/= pomLink)) tags
    case jarOrPomAsText of
      Just tag -> pure tag
      -- fallback to .asc files as in some cases the latest release will not have a pom or jar..
      Nothing -> (!!? 3) $ take 4 $ dropWhile (\a -> (a T.~/= jarAscLink) && (a T.~/= pomAscLink)) tags

  let statsText = decodeUtf8 $ T.innerText [statsTag]
  first (const $ "Failure to parse stats text: " <> statsText) $ M.parse parseTimeSegment "Maven Release Time" statsText

parseTimeSegment :: MParser UTCTime
parseTimeSegment = do
  M.hspace
  timeAsText <- M.someTill timeChars (M.string "  ")
  parseTimeM False defaultTimeLocale "%Y-%-m-%-d %H:%M" timeAsText

timeChars :: MParser Char
timeChars = M.numberChar <|> M.char ':' <|> M.char '-' <|> M.char ' '
