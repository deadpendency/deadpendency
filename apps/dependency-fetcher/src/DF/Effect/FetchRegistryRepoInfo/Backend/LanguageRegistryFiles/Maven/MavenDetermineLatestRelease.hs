module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Maven.MavenDetermineLatestRelease
  ( determineLatestRelease,
  )
where

import Common.Parsing.Megaparsec
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NV
import Data.Versions qualified as DV
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

determineLatestRelease :: NV.NonEmptyVector Text -> Maybe Text
determineLatestRelease inputVec = do
  let validReleaseVersions = NV.filter (isRight . DV.versioning) inputVec
      notBetaReleases = V.filter (isJust . mParseMaybe parseReleaseVersion) validReleaseVersions
      finalList = concatMaybeV $ fmap (rightToMaybe . DV.versioning) notBetaReleases
  DV.prettyV <$> safeMaximumV finalList

parseReleaseVersion :: MParser ()
parseReleaseVersion = do
  M.some (M.digitChar <|> M.char '.' <|> M.char '-')
  M.optional $ M.string' "ga" <|> M.string' "release" <|> M.string' "final"
  M.eof
