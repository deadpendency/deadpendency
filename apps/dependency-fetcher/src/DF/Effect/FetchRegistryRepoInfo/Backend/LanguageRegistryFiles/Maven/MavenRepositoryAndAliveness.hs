module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenRepositoryAndAliveness
  ( MavenRepositoryAndAliveness (..),
    fetchRepositoryAndAliveness,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Git.Repo
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import Common.Parsing.Xml
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.Version.MavenLatestVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Vector qualified as V
import Network.HTTP.Req
import Text.XML.Hexml qualified as HX
import Text.XML.Hexml.Lens qualified as HX

fetchRepositoryAndAliveness :: Text -> Text -> MavenLatestVersion -> ExceptT FetchDependencyRegistryError IO MavenRepositoryAndAliveness
fetchRepositoryAndAliveness namespace name latestVersion = do
  let versionText = latestVersion ^. #_version
      maybeScalaVersion = latestVersion ^. #_maybeScalaVersion
      -- gross hack because they renamed the old pom file not the new one
      -- https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.31/
      nameWithMysqlHack =
        if name == "mysql-connector-java"
          then "mysql-connector-j"
          else name
      depNameWithScala = maybe nameWithMysqlHack (\scalaVersion -> nameWithMysqlHack <> "_" <> scalaVersion) maybeScalaVersion
      slug = depNameWithScala <> "-" <> versionText <> ".pom"
      -- https://repo1.maven.org/maven2/org/hibernate/hibernate-core/5.4.9.Final/hibernate-core-5.4.9.Final.pom
      url = getUrlBaseWithVersion namespace name maybeScalaVersion /: versionText /: slug
  maybeXmlResult <- ExceptT $ fetchUrl url
  case maybeXmlResult of
    Nothing -> pure $ MavenRepositoryAndAliveness RASAlive Nothing
    Just xmlResult -> do
      let strippedCData = stripCDataSections xmlResult
      docNode <- hoistEither $ first (FDRFailureToParseResult . decodeUtf8) $ HX.parse strippedCData
      let projectRoot = docNode ^.. projectFromRoot
          repositories = fmap (mParseMaybe parserRepo) $ V.fromList $ projectRoot ^.. (folded . repositoryFromMetaData)
          projectUrlsAsQR = fmap (mParseMaybe parserQualifiedRepo) $ V.fromList $ projectRoot ^.. (folded . projectUrlFromMetaData)
          maybeRepo = selectFinalRepo repositories projectUrlsAsQR
          maybeTheseRelocation = headMaybe $ projectRoot ^.. (folded . relocationFromMetaData)
          aliveness =
            case maybeTheseRelocation of
              Nothing -> RASAlive
              Just theseRelocation -> RASDeprecated RASTRelocated Nothing $ V.singleton $ produceRelocatedName namespace name theseRelocation

      pure $
        MavenRepositoryAndAliveness aliveness maybeRepo

data MavenRepositoryAndAliveness = MavenRepositoryAndAliveness
  { _aliveness :: RegistryAlivenessStatus,
    _repo :: Maybe Repo
  }
  deriving stock (Show, Generic)

{-
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <url>https://spring.io/projects/spring-boot</url>
  <scm>
    <connection>scm:git:git://github.com/spring-projects/spring-boot.git</connection>
    <developerConnection>scm:git:ssh://git@github.com/spring-projects/spring-boot.git</developerConnection>
    <url>https://github.com/spring-projects/spring-boot</url>
  </scm>
  <distributionManagement>
    <relocation>
      <groupId>org.hibernate.orm</groupId>
      <artifactId>hibernate-core</artifactId>
      <version>6.0.0.Alpha8</version>
    </relocation>
  </distributionManagement>
</project>

-}

projectFromRoot :: Fold HX.Node HX.Node
projectFromRoot =
  HX.node @Text "project"

repositoryFromMetaData :: Fold HX.Node Text
repositoryFromMetaData =
  HX.node @Text "scm"
    . HX.node @Text "url"
    . HX._inner

projectUrlFromMetaData :: Fold HX.Node Text
projectUrlFromMetaData =
  HX.node @Text "url"
    . HX._inner

relocationFromMetaData :: Fold HX.Node (These Text Text)
relocationFromMetaData =
  HX.node @Text "distributionManagement"
    . HX.node @Text "relocation"
    . folding (\s -> maybesToThese (s ^? groupId) (s ^? artifactId))

groupId :: Fold HX.Node Text
groupId =
  HX.node @Text "groupId"
    . HX._inner

artifactId :: Fold HX.Node Text
artifactId =
  HX.node @Text "artifactId"
    . HX._inner

produceRelocatedName :: Text -> Text -> These Text Text -> DependencyName
produceRelocatedName oldNamespace oldName theseNewNamespaceName =
  DependencyName $
    case theseNewNamespaceName of
      This newNamespace -> newNamespace <> "/" <> oldName
      That newName -> oldNamespace <> "/" <> newName
      These newNamespace newName -> newNamespace <> "/" <> newName
