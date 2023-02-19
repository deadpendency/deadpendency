module Common.HtmlReport.Instances.Internal
  ( filteredLiFromId,
    partitionsFromIdByTag,
    splitByLiTags,
    getTagInnerContent,
    getTagInnerContentText,
    getTagContentWithId,
    renderSimpleRepoHost,
    qrToHref,
    getNextTagContent,
    tagHasIdPrefix,
  )
where

import Common.Model.Git.QualifiedRepo
import Common.Model.Git.RepoHost
import Text.HTML.TagSoup qualified as T
import Text.HTML.TagSoup.Match qualified as T
import Text.HTML.TagSoup.Tree qualified as T

filteredLiFromId :: Text -> [T.Tag Text] -> [[T.Tag Text]]
filteredLiFromId matchingId tags =
  filter listTagsContainsId (splitByLiTags tags)
  where
    listTagsContainsId :: [T.Tag Text] -> Bool
    listTagsContainsId = any (T.tagOpen (== "li") (T.anyAttr $ idHasSuffix matchingId))

idHasSuffix :: Text -> (Text, Text) -> Bool
idHasSuffix matchingId (key, value) = key == "id" && (matchingId `isSuffixOf` value)

tagHasIdPrefix :: Text -> Text -> T.Tag Text -> Bool
tagHasIdPrefix tagName matchingId = T.tagOpenLit tagName (any (idHasPrefix matchingId))

idHasPrefix :: Text -> (Text, Text) -> Bool
idHasPrefix matchingId (key, value) = key == "id" && (matchingId `isPrefixOf` value)

getTagContentWithId :: Text -> Text -> [T.Tag Text] -> [T.Tag Text]
getTagContentWithId name matchingId =
  getNextTagContent . dropWhile (not . T.tagOpenLit name (any (idHasSuffix matchingId)))

getNextTagContent :: [T.Tag Text] -> [T.Tag Text]
getNextTagContent = T.flattenTree . take 1 . T.tagTree

getTagInnerContent :: Text -> [T.Tag Text] -> [T.Tag Text]
getTagInnerContent name input =
  let outerTagTreeList = (take 1 . T.tagTree . dropWhile (not . T.isTagOpenName name)) input
   in case outerTagTreeList of
        [] -> []
        (x : _) -> T.flattenTree $ getBranchChildren x

getTagInnerContentText :: Text -> [T.Tag Text] -> Text
getTagInnerContentText tagName = T.renderTags . getTagInnerContent tagName

splitByLiTags :: [T.Tag Text] -> [[T.Tag Text]]
splitByLiTags input =
  let ulAsList = (take 1 . T.tagTree . dropWhile (not . T.isTagOpenName "ul")) input
   in case ulAsList of
        [] -> []
        (x : _) -> T.flattenTree . one <$> getBranchChildren x

getBranchChildren :: T.TagTree str -> [T.TagTree str]
getBranchChildren (T.TagLeaf _) = []
getBranchChildren (T.TagBranch _ _ children) = children

partitionsFromIdByTag :: Text -> Text -> [T.Tag Text] -> [[T.Tag Text]]
partitionsFromIdByTag matchingTag matchingId input =
  let allTagsAtLevel = (T.tagTree . dropWhile (not . T.tagOpen (== matchingTag) (T.anyAttr $ idHasSuffix matchingId))) input
      levelTagsThatMatch = filter (treeMatchesNameAndId matchingTag matchingId) allTagsAtLevel
   in T.flattenTree . one <$> levelTagsThatMatch

treeMatchesNameAndId :: Text -> Text -> T.TagTree Text -> Bool
treeMatchesNameAndId _ _ (T.TagLeaf _) = False
treeMatchesNameAndId matchingTag matchingId (T.TagBranch tagName attributes _) =
  tagName == matchingTag
    && (T.anyAttr $ idHasSuffix matchingId) attributes

qrToHref :: QualifiedRepo -> Text
qrToHref qr =
  let repoHost = qr ^. #_repoHost
      owner = qr ^. (#_repoOwner . #_ntText)
      name = qr ^. (#_repoName . #_ntText)
      repoHostText = renderSimpleRepoHost repoHost
   in "https://" <> repoHostText <> "/" <> owner <> "/" <> name

renderSimpleRepoHost :: RepoHost -> Text
renderSimpleRepoHost =
  \case
    GitHub -> "github.com"
    Bitbucket -> "bitbucket.org"
    GitLab -> "gitlab.com"
