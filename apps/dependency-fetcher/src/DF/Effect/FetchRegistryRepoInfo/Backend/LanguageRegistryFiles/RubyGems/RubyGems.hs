module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGems
  ( fetchDependencyRubyGems,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGemsPackage
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGemsVersion
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Network.HTTP.Req

fetchDependencyRubyGems :: DependencyName -> IO (Either FetchDependencyRegistryError (Maybe DependencyRegistryInfo))
fetchDependencyRubyGems dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText
      baseUrl = https "rubygems.org" /: "api"
      versionUrl = baseUrl /: "v1" /: "versions" /: dependencyNameText /: "latest.json"
  eitherMaybeRubyGemsVersion <- fetchJSON @RubyGemsVersion versionUrl
  case eitherMaybeRubyGemsVersion of
    Right (Just (RubyGemsVersion version)) -> do
      let packageUrl = baseUrl /: "v2" /: "rubygems" /: dependencyNameText /: "versions" /: (version <> ".json")
      eitherMaybeRubyGemsPackage <- fetchJSON @RubyGemsPackage packageUrl
      pure $ eitherMaybeRubyGemsPackage <<&>> _result
    Right Nothing -> pure $ Right Nothing
    Left e -> pure $ Left e
