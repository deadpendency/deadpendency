{-# LANGUAGE DataKinds #-}

module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetAdditionalDetails
  ( NuGetAdditionalDetails (..),
    fetchAdditionalDetails,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetLatestVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson
import Data.Vector qualified as V
import Network.HTTP.Req
import Text.URI qualified as TURI

fetchAdditionalDetails :: NuGetLatestVersion -> DependencyName -> ExceptT FetchDependencyRegistryError IO NuGetAdditionalDetails
fetchAdditionalDetails latestVersion dependencyName = do
  catalogEntryUrl <- fetchCatalogEntryUrl latestVersion dependencyName
  fetchAdditionalDetailsInternal dependencyName catalogEntryUrl

fetchCatalogEntryUrl :: NuGetLatestVersion -> DependencyName -> ExceptT FetchDependencyRegistryError IO NuGetCatalogEntryUrl
fetchCatalogEntryUrl latestVersion dependencyName = do
  let dependencyNameText = toLower $ dependencyName ^. #_ntText
      latestVersionText = toLower $ latestVersion ^. #_ntText
      url = https "api.nuget.org" /: "v3" /: "registration5-gz-semver2" /: dependencyNameText /: latestVersionText <> ".json"
  maybeCatalogEntryUrl <- ExceptT $ fetchJSONRetry404 2 url -- catalog page is a bit flakey
  case maybeCatalogEntryUrl of
    Just catalogEntryUrl -> pure catalogEntryUrl
    Nothing -> hoistEither $ Left $ FDRRegistryDataInconsistent $ "Unexpected missing catalog page for: " <> dependencyNameText

newtype NuGetCatalogEntryUrl = NuGetCatalogEntryUrl
  { _ntText :: Text
  }
  deriving stock (Show, Generic)

{-
{
  "catalogEntry": "https://api.nuget.org/v3/catalog0/data/2020.08.10.20.50.59/nuget.core.2.14.0.json"
}
-}

instance FromJSON NuGetCatalogEntryUrl where
  parseJSON =
    withObject "NuGetCatalogEntryUrl" $ \v -> do
      catalogEntry <- v .: "catalogEntry"
      pure $ NuGetCatalogEntryUrl catalogEntry

-- Final Additional Details

fetchAdditionalDetailsInternal :: DependencyName -> NuGetCatalogEntryUrl -> ExceptT FetchDependencyRegistryError IO NuGetAdditionalDetails
fetchAdditionalDetailsInternal dependencyName catalogEntryUrl = do
  let dependencyNameText = dependencyName ^. #_ntText
      catalogEntryUrlText = catalogEntryUrl ^. #_ntText
  url <- hoistEither $ maybeToRight (FDRFailureToParseResult $ "Unexpected unparsable catalogEntry: " <> catalogEntryUrlText <> " for " <> dependencyNameText) (textToUrl catalogEntryUrlText)
  maybeAdditionalDetails <- ExceptT $ fetchJSON @NuGetAdditionalDetails url
  case maybeAdditionalDetails of
    Just additionalDetails -> pure additionalDetails
    Nothing -> hoistEither $ Left $ FDRRegistryDataInconsistent $ "Unexpected missing additional details page for: " <> dependencyNameText

textToUrl :: Text -> Maybe (Url 'Https)
textToUrl textUrl = do
  uri <- rightToMaybe $ TURI.mkURI textUrl
  fst <$> useHttpsURI uri

data NuGetAdditionalDetails = NuGetAdditionalDetails
  { _deprecated :: RegistryAlivenessStatus,
    _maybeReleaseDateTime :: Maybe UTCTime
  }
  deriving stock (Show, Generic)

{-
{
  # key missing if not deprecated
  "deprecation": {
    "@id": "https://api.nuget.org/v3/catalog0/data/2020.08.10.20.50.59/nuget.core.2.14.0.json#deprecation",
    "message": "NuGet.Core is part of NuGet client v2 APIs. They have been replaced by NuGet client v3 and later APIs. https://docs.microsoft.com/en-us/nuget/reference/nuget-client-sdk",
  "alternatePackage": {
    "@id": "https://api.nuget.org/v3/catalog0/data/2019.10.04.00.24.50/nuget.protocol.core.types.4.2.0.json#deprecation/alternatePackage",
    "id": "NuGet.Protocol",
    "range": "*"
  },
    "reasons": [
    "Legacy"
    ]
  },
  "published": "2016-12-14T23:46:00.827Z",
}
-}

instance FromJSON NuGetAdditionalDetails where
  parseJSON =
    withObject "NuGetAdditionalDetails" $ \v -> do
      rawPublishedTime <- v .: "published"
      let (year, _, _) = toGregorian $ utctDay rawPublishedTime
          -- there are edge cases where the published year is 1900, which is cleary invalid https://api.nuget.org/v3/catalog0/data/2021.02.16.21.27.00/microsoft.visualstudio.azure.containers.tools.targets.1.10.14.json
          -- we replace these with Nothing
          maybePublishedTime =
            if year > 1900
              then Just rawPublishedTime
              else Nothing
      maybeDeprecation <- v .:? "deprecation"
      deprecated <-
        case maybeDeprecation of
          Just deprecation -> do
            maybeMessage <- deprecation .:? "message"
            maybeAlternatePackage <- deprecation .:? "alternatePackage"
            deprecatedForPackages <-
              case maybeAlternatePackage of
                Nothing -> pure V.empty
                Just alternatePackage -> V.singleton . DependencyName <$> alternatePackage .: "id"
            pure $ RASDeprecated RASTDeprecated maybeMessage deprecatedForPackages
          Nothing -> pure RASAlive

      pure $ NuGetAdditionalDetails deprecated maybePublishedTime
