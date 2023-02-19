module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetRepository
  ( fetchSourceRepository,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Git.Repo
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetLatestVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Vector qualified as V
import Network.HTTP.Req
import Text.XML.Hexml qualified as HX
import Text.XML.Hexml.Lens qualified as HX

fetchSourceRepository :: NuGetLatestVersion -> DependencyName -> ExceptT FetchDependencyRegistryError IO (Maybe Repo)
fetchSourceRepository latestVersion dependencyName = do
  let dependencyNameText = toLower $ dependencyName ^. #_ntText
      latestVersionText = toLower $ latestVersion ^. #_ntText
      url = https "api.nuget.org" /: "v3-flatcontainer" /: dependencyNameText /: latestVersionText /: dependencyNameText <> ".nuspec"
  maybeXmlResult <- ExceptT $ fetchUrl url
  case maybeXmlResult of
    Nothing -> pure Nothing
    Just xmlResult -> do
      docNode <- hoistEither $ first (FDRFailureToParseResult . decodeUtf8) $ HX.parse xmlResult
      let metadataRoot = docNode ^.. metadataFromRoot
          repositories = fmap (mParseMaybe parserRepo) $ V.fromList $ metadataRoot ^.. (folded . repositoryFromMetaData)
          projectUrlsAsQR = fmap (mParseMaybe parserQualifiedRepo) $ V.fromList $ metadataRoot ^.. (folded . projectUrlFromMetaData)
          maybeResult = selectFinalRepo repositories projectUrlsAsQR

      pure maybeResult

{-
<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
  <metadata>
    <projectUrl>http://www.castleproject.org/</projectUrl>
    <repository type="git" url="https://github.com/castleproject/Core"/>
  </metadata>
</package>
-}

metadataFromRoot :: Fold HX.Node HX.Node
metadataFromRoot =
  HX.node @Text "package"
    . HX.node @Text "metadata"

repositoryFromMetaData :: Fold HX.Node Text
repositoryFromMetaData =
  HX.node @Text "repository"
    . filteredBy (HX._Attribute @Text "type" . only (Just "git"))
    . HX._Attribute @Text "url"
    . _Just

projectUrlFromMetaData :: Fold HX.Node Text
projectUrlFromMetaData =
  HX.node @Text "projectUrl"
    . HX._inner @Text
