module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.HackageLatestRevision
  ( HackageLatestRevision (..),
    fetchLatestReleaseTime,
  )
where

import Common.Model.Dependency.DependencyName
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import Network.HTTP.Req

-- curl -H "Accept: application/json" https://hackage.haskell.org/package/these/revisions/
fetchLatestReleaseTime :: DependencyName -> ExceptT FetchDependencyRegistryError IO (Maybe UTCTime)
fetchLatestReleaseTime dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText
      -- it will 301 redirect without the trailing slash
      url = https "hackage.haskell.org" /: "package" /: dependencyNameText /: "revisions" /: ""
  maybeHackageLatestRevision <- ExceptT $ fetchJSON @HackageLatestRevision url
  pure $ maybeHackageLatestRevision <&> \h -> h ^. #_result

newtype HackageLatestRevision = HackageLatestRevision
  { _result :: UTCTime
  }
  deriving stock (Show, Generic)

{-
[
  {
    "user": "phadej",
    "number": 0,
    "time": "2020-07-14T18:40:29Z"
  },
  {
    "user": "phadej",
    "number": 1,
    "time": "2021-02-14T23:05:34Z"
  }
]
-}

instance FromJSON HackageLatestRevision where
  parseJSON =
    withArray "HackageLatestRevision" $ \values -> do
      time <-
        case getNumberZero values of
          Just firstRelease -> withObject "FirstRelease" (.: "time") firstRelease
          Nothing -> fail $ "Unexpected no releases to hackage: " <> show values
      pure $
        HackageLatestRevision time

getNumberZero :: V.Vector Value -> Maybe Value
getNumberZero = V.find $ \case
  (Object hm) -> KM.lookup "number" hm == Just (Number 0)
  _ -> False
