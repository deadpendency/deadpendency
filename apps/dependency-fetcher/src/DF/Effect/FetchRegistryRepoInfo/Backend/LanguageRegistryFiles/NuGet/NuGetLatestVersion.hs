module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.NuGet.NuGetLatestVersion
  ( NuGetLatestVersion (..),
    fetchLatestVersion,
  )
where

import Common.Model.Dependency.DependencyName
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson
import Network.HTTP.Req

fetchLatestVersion :: DependencyName -> ExceptT FetchDependencyRegistryError IO (Maybe NuGetLatestVersion)
fetchLatestVersion dependencyName = do
  let dependencyNameText = toLower $ dependencyName ^. #_ntText
      url = https "api.nuget.org" /: "v3-flatcontainer" /: dependencyNameText /: "index.json"
  maybePossibleLatestVersion <- ExceptT $ fetchJSON @PossibleNuGetLatestVersion url
  pure $
    maybePossibleLatestVersion >>= \pv -> (pv ^. #_result) <&> NuGetLatestVersion

newtype NuGetLatestVersion = NuGetLatestVersion
  { _ntText :: Text
  }
  deriving stock (Show, Generic)

newtype PossibleNuGetLatestVersion = PossibleNuGetLatestVersion
  { _result :: Maybe Text
  }
  deriving stock (Show, Generic)

{-
{
  "versions": [
    "1.1.0",
    "1.2.0",
    "2.5.1",
    "3.0.0.2001",
    "3.0.0.3001",
    "3.0.0.4001",
    "3.1.0-rc",
    "3.1.0",
    "4.0.0-alpha001",
    "4.0.0-beta001",
    "4.0.0-beta002",
    "4.0.0",
    "4.4.1"
  ]
}
-}

instance FromJSON PossibleNuGetLatestVersion where
  parseJSON =
    withObject "PossibleNuGetLatestVersion" $ \v -> do
      versions <- v .: "versions"
      pure $ PossibleNuGetLatestVersion (getLatestReleaseKey versions)
