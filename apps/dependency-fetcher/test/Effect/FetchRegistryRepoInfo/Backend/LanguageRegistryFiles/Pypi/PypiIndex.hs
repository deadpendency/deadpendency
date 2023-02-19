module Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Pypi.PypiIndex
  ( fetchPackageNames,
  )
where

import Common.Model.Dependency.DependencyName
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Vector qualified as V
import Network.HTTP.Req
import Text.HTML.TagSoup qualified as T

fetchPackageNames :: Int -> Int -> IO (V.Vector DependencyName)
fetchPackageNames count skip = do
  let url = https "pypi.org" /: "simple"
  eitherMaybePypiIndexAsBs <- fetchUrl url
  pure $
    case eitherMaybePypiIndexAsBs of
      Right (Just pypiIndexBs) -> getDependencies count skip pypiIndexBs
      Right Nothing -> error "got no results"
      Left a -> error $ "boom: " <> show a

{-
<!DOCTYPE html>
<html>
  <head>
    <title>Simple index</title>
  </head>
  <body>
    <a href="/simple/0/">0</a>
    <a href="/simple/0-0/">0-._.-._.-._.-._.-._.-._.-0</a>
    <a href="/simple/00000a/">00000a</a>
    <a href="/simple/0-0-1/">0.0.1</a>
  </body>
</html>
-}

getDependencies :: Int -> Int -> ByteString -> V.Vector DependencyName
getDependencies count skip input =
  let tags = T.parseTags input
      anchorTags = T.partitions (T.isTagOpenName "a") tags
      toFetchAnchorTags = take count $ drop skip anchorTags
      finalList = T.innerText <$> toFetchAnchorTags
      result = DependencyName . stripEnd . decodeUtf8 <$> V.fromList finalList
   in result
