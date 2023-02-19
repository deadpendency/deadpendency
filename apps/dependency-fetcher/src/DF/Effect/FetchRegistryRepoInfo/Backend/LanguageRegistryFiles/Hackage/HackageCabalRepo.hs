module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Hackage.HackageCabalRepo
  ( fetchCabalRepo,
    HackageCabalRepo (..),
  )
where

import Common.Model.Dependency.DependencyName
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import DF.Effect.FetchRegistryRepoInfo.Backend.Model.FetchDependencyRegistryError
import Data.Vector qualified as V
import Distribution.PackageDescription.Parsec qualified as C
import Distribution.Types.GenericPackageDescription qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.SourceRepo qualified as C
import Distribution.Utils.ShortText qualified as C
import Network.HTTP.Req

{-
cabal-version:   1.18
name:            req
version:         3.3.0
homepage:        https://github.com/mrkkrp/req

source-repository head
    type:     git
    location: https://github.com/mrkkrp/req.git
-}

fetchCabalRepo :: DependencyName -> ExceptT FetchDependencyRegistryError IO (Maybe HackageCabalRepo)
fetchCabalRepo dependencyName = do
  let dependencyNameText = dependencyName ^. #_ntText
      url = https "hackage.haskell.org" /: "package" /: dependencyNameText /: dependencyNameText <> ".cabal"
  maybeBytestringResult <- ExceptT $ fetchUrl url
  hoistEither $ first FDRFailureToParseResult (traverse parseHackageCabalRepo maybeBytestringResult)

newtype HackageCabalRepo = HackageCabalRepo
  { _result :: Maybe Repo
  }
  deriving stock (Show, Generic)

parseHackageCabalRepo :: ByteString -> Either Text HackageCabalRepo
parseHackageCabalRepo bsInput = do
  gpd <- toNiceError $ C.parseGenericPackageDescription bsInput
  let packageDesc = gpd ^. #packageDescription
      maybeSourceRepo = getSourceRepo packageDesc
      maybeHomepageRepo = getHomepageRepo packageDesc
      finalMaybeRepo = selectFinalRepo (V.singleton maybeSourceRepo) (V.singleton maybeHomepageRepo)

  pure $ HackageCabalRepo finalMaybeRepo

getSourceRepo :: C.PackageDescription -> Maybe Repo
getSourceRepo packageDesc =
  let maybeGitRepoText = packageDesc ^.. (#sourceRepos . folded . to getGitRepoUrl . _Just) ^? ix 0
   in maybeGitRepoText >>= mParseMaybe parserRepo

getHomepageRepo :: C.PackageDescription -> Maybe QualifiedRepo
getHomepageRepo packageDesc =
  let homepageText = packageDesc ^. (#homepage . to (pack . C.fromShortText))
   in mParseMaybe parserQualifiedRepo homepageText

getGitRepoUrl :: C.SourceRepo -> Maybe Text
getGitRepoUrl C.SourceRepo {C.repoLocation = Just ""} = Nothing
getGitRepoUrl C.SourceRepo {C.repoLocation = Just repoUrlString} = Just $ pack repoUrlString
getGitRepoUrl _ = Nothing

toNiceError :: C.ParseResult C.GenericPackageDescription -> Either Text C.GenericPackageDescription
toNiceError = first (("Unable to parse cabal file: " <>) . show) . snd . C.runParseResult
