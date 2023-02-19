module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.NpmPackage
  ( NpmPackage (..),
    parseNpmPackage,
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.Repo
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Aeson qualified as A
import Data.Aeson.Types (FromJSON (..), Value (..), prependFailure, typeMismatch)
import Data.ByteString.Lazy.Internal qualified as BL
import Data.JsonStream.Parser
import Data.Vector qualified as V

newtype NpmPackage = NpmPackage
  { _result :: DependencyRegistryInfo
  }
  deriving stock (Show)

{-
"dist-tags": {
  "latest": "1.8.1"
},
"versions": {
  "1.8.1": {
    "deprecated": "Package no longer supported. Contact support@npmjs.com for more info."
  }
},
"time": {
  "modified": "2018-03-17T02:16:35.116Z",
  "created": "2011-04-07T23:03:55.183Z",
  "1.8.1": "2014-11-07T18:17:52.473Z",
  "2.0.0": "2018-02-17T00:32:37.653Z"
},
-}

parseNpmPackage :: DependencyName -> LByteString -> Either FetchDependencyRegistryError NpmPackage
parseNpmPackage dependencyName input = do
  maybeLatestVersion <- parseOutputToEither dependencyName $ runParser'' npmLatestVersionParser input
  parseOutputToEither dependencyName $ runParser'' (npmPackageParser maybeLatestVersion) input

-- the library doesn't have a simple LBS version
runParser'' :: Parser a -> LByteString -> ParseOutput a
runParser'' parser input = loop input (runParser parser)
  where
    loop BL.Empty (ParseNeedData _) = ParseFailed "Not enough data."
    loop (BL.Chunk dta rest) (ParseNeedData np) = loop rest (np dta)
    loop _ other = other

parseOutputToEither :: DependencyName -> (forall a. ParseOutput a -> Either FetchDependencyRegistryError a)
parseOutputToEither dependencyName parseOutput =
  let depNameText = dependencyName ^. #_ntText
   in case parseOutput of
        ParseYield a _ -> Right a
        ParseFailed failure -> Left $ FDRFailureToParseResult $ "Failure to parse: " <> depNameText <> " with: " <> pack failure
        _ -> Left $ FDRFailureToParseResult $ "No Parse result: " <> depNameText

npmLatestVersionParser :: Parser (Maybe Text)
npmLatestVersionParser = optional $ "dist-tags" .: "latest" .: string

npmPackageParser :: Maybe Text -> Parser NpmPackage
npmPackageParser maybeLatestVersion = do
  maybeRepo <- join <$> (_maybeRepo <<$>> "repository" .:? value)

  (aliveness, maybeLatestReleaseTime) <-
    case maybeLatestVersion of
      Nothing -> pure (RASAlive, Nothing)
      Just latestReleaseVersion ->
        (,)
          <$> ( ("versions" .: latestReleaseVersion .: "deprecated" .:? value)
                  <&> \case
                    Just (NpmAlivenessStatus aliveness') -> aliveness'
                    Nothing -> RASAlive
              )
          <*> optional ("time" .: latestReleaseVersion .: value)

  pure $
    NpmPackage $
      DependencyRegistryInfo
        Npm
        maybeRepo
        aliveness
        maybeLatestReleaseTime

newtype NpmQualifiedRepo = NpmQualifiedRepo
  { _maybeRepo :: Maybe Repo
  }

instance FromJSON NpmQualifiedRepo where
  parseJSON repository = do
    maybeUrl <-
      case repository of
        (A.String s) -> pure $ Just s
        (A.Object repoObject) -> repoObject A..:? "url"
        unmatched ->
          prependFailure "parsing Repository failed, " (typeMismatch "Object / String" unmatched)

    pure $
      NpmQualifiedRepo $
        maybeUrl >>= mParseMaybe parserRepo

newtype NpmAlivenessStatus = NpmAlivenessStatus
  { _aliveness :: RegistryAlivenessStatus
  }

instance FromJSON NpmAlivenessStatus where
  parseJSON deprecated = do
    aliveness <-
      case deprecated of
        (String deprecationReason) -> pure $ RASDeprecated RASTDeprecated (Just deprecationReason) V.empty
        (Bool True) -> pure $ RASDeprecated RASTDeprecated Nothing V.empty
        (Bool False) -> pure RASAlive
        invalid ->
          prependFailure
            "parsing deprecated value failed, "
            (typeMismatch "Object / String" invalid)

    pure $
      NpmAlivenessStatus aliveness
