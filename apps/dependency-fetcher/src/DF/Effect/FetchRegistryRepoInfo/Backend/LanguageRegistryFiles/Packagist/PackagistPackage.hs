module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Packagist.PackagistPackage
  ( PackagistPackage (..),
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing (depNameChar)
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Text qualified as Text
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

newtype PackagistPackage = PackagistPackage
  { _result :: DependencyRegistryInfo
  }
  deriving stock (Show)

{-
  {
    "package": {
      "repository": "https://github.com/Seldaek/monolog"
      "versions": {
        "1.0.0": {}
        "1.2.0": {}
        "2.1.0": {
          "time": "2020-05-22T08:12:19+00:00",
          "abandoned": true / "other/monolog"
        }
      }
    }
  }
-}

instance FromJSON PackagistPackage where
  parseJSON =
    withObject "PackagistPackage" $ \v -> do
      package <- v .: "package"
      repoUrl <- package .: "repository"
      let maybeRepoDetails =
            mParseMaybe parserRepo repoUrl

      versions <- package .: "versions"
      -- some packagist packages prefix with v, breaking semver https://packagist.org/packages/laravel/framework
      let textVersions = fmap (stripVPrefix . KM.toText) (V.fromList $ KM.keys versions)
          maybeLatestKey = getLatestReleaseKey textVersions
      (maybeLatestReleaseTime, maybeAbandoned) <-
        case maybeLatestKey of
          Just latestReleaseKey -> do
            -- we may have stripped the v, so we need to try a version with v prefixed if the pure version fails
            release <- (versions .: KM.fromText latestReleaseKey) <|> (versions .: KM.fromText (Text.cons 'v' latestReleaseKey))
            releaseTime <- release .: "time"
            maybeAbandoned <- release .:? "abandoned"
            pure (Just releaseTime, maybeAbandoned)
          Nothing -> pure (Nothing, Nothing)

      aliveness <-
        case maybeAbandoned of
          Just (Bool True) -> pure $ RASDeprecated RASTAbandoned Nothing V.empty
          Just (String abandonedDetails) ->
            -- the string is supposed to be a new dep, but can sometimes be a message.
            -- we attempt to match a dep name first
            case mParseMaybe parserPackigistDep abandonedDetails of
              Just depName -> pure $ RASDeprecated RASTAbandoned Nothing $ V.singleton depName
              Nothing -> pure $ RASDeprecated RASTAbandoned (Just abandonedDetails) V.empty
          Just invalid ->
            prependFailure
              "parsing abandoned value failed, "
              (typeMismatch "Object / String" invalid)
          _ -> pure RASAlive

      pure $
        PackagistPackage $
          DependencyRegistryInfo
            Packagist
            maybeRepoDetails
            aliveness
            maybeLatestReleaseTime

parserPackigistDep :: MParser DependencyName
parserPackigistDep = do
  firstPart <- pack <$> M.someTill depNameChar (M.char '/')
  secondPart <- pack <$> M.someTill depNameChar M.eof
  pure $
    DependencyName $
      firstPart <> "/" <> secondPart
