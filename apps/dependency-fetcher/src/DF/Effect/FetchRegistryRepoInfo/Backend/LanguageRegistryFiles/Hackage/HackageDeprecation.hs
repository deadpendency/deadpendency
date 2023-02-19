module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.HackageDeprecation
  ( HackageDeprecation (..),
    fetchPackageDeprecation,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson
import Network.HTTP.Req

fetchPackageDeprecation :: DependencyName -> ExceptT FetchDependencyRegistryError IO (Maybe RegistryAlivenessStatus)
fetchPackageDeprecation dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText
      url = https "hackage.haskell.org" /: "package" /: dependencyNameText /: "deprecated"
  maybeHackageDeprecation <- ExceptT $ fetchJSON @HackageDeprecation url
  pure $ maybeHackageDeprecation <&> \h -> h ^. #_result

newtype HackageDeprecation = HackageDeprecation
  { _result :: RegistryAlivenessStatus
  }
  deriving stock (Show, Generic)

{-
  {"in-favour-of":["Cabal"],"is-deprecated":true}
  {"in-favour-of":[],"is-deprecated":false}
-}

instance FromJSON HackageDeprecation where
  parseJSON =
    withObject "HackageDeprecation" $ \v -> do
      isDeprecated <- v .: "is-deprecated"
      HackageDeprecation
        <$> if isDeprecated
          then v .: "in-favour-of" <&> RASDeprecated RASTDeprecated Nothing . fmap DependencyName
          else pure RASAlive
