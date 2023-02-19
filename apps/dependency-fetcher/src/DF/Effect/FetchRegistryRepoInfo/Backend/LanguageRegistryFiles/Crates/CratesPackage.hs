module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Crates.CratesPackage
  ( CratesPackage (..),
  )
where

import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Aeson
import Data.Vector qualified as V

newtype CratesPackage = CratesPackage
  { _result :: DependencyRegistryInfo
  }
  deriving stock (Show)

{-
{
  "crate": {
    "homepage": "https://crates.io/crates/rand",
    "repository": "https://github.com/rust-random/rand"
    "max_version": "0.7.3",
  }

  "versions": [
    {
      "num": "0.7.3",
      "created_at": "2020-01-10T21:46:21.337656+00:00",
    }
  ]
}

-}

instance FromJSON CratesPackage where
  parseJSON =
    withObject "CratesPackage" $ \v -> do
      crate <- v .: "crate"
      maybeRepository <- crate .:? "repository"
      maybeHomepage <- crate .:? "homepage"
      let maybeSourceRepo = maybeRepository >>= mParseMaybe parserRepo
          maybeHomepageQR = maybeHomepage >>= mParseMaybe parserQualifiedRepo
      let maybeRepo =
            selectFinalRepo (V.singleton maybeSourceRepo) (V.singleton maybeHomepageQR)

      maxVersion <- crate .: "max_version"
      versions <- v .: "versions"
      let maybeLatest = V.find (\version -> _versionText version == maxVersion) versions
          maybeLatestTime = maybeLatest <&> _createdAt

      pure $
        CratesPackage $
          DependencyRegistryInfo
            Crates
            maybeRepo
            RASAlive
            maybeLatestTime

instance FromJSON RawCrateVersion where
  parseJSON =
    withObject "RawCrateVersion" $ \v -> do
      version <- v .: "num"
      createdAt <- v .: "created_at"
      pure $
        RawCrateVersion
          { _versionText = version,
            _createdAt = createdAt
          }

data RawCrateVersion = RawCrateVersion
  { _versionText :: Text,
    _createdAt :: UTCTime
  }
