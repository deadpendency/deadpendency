module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGemsPackage
  ( RubyGemsPackage (..),
  )
where

import Common.Model.Dependency.Registry.DependencyRegistryInfo
import Common.Model.Dependency.Registry.RegistryAlivenessStatus
import Common.Model.Ecosystem.Registry
import Common.Model.Git.QualifiedRepo
import Common.Model.Git.Repo
import Common.Parsing.Megaparsec
import Common.Parsing.RepoParsing
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Internal
import Data.Aeson
import Data.Vector qualified as V
import Text.Megaparsec qualified as M

newtype RubyGemsPackage = RubyGemsPackage
  { _result :: DependencyRegistryInfo
  }
  deriving stock (Show)

{-
  {
    "homepage_uri": "https://rubyonrails.org",
    "source_code_uri": "https://github.com/rails/rails/tree/v6.0.3.1",
    "created_at": "2020-05-18T15:47:58.979Z"
  }
-}

instance FromJSON RubyGemsPackage where
  parseJSON =
    withObject "RubyGemsPackage" $ \v -> do
      maybeSourceCodeUri <- v .: "source_code_uri"
      maybeHomepage <- v .: "homepage_uri"
      let maybeSourceRepo = maybeSourceCodeUri >>= mParseMaybe parserRubyGemsRepo
          maybeHomepageQR = maybeHomepage >>= mParseMaybe parserRubyGemsQR
      let maybeRepo =
            selectFinalRepo (V.singleton maybeSourceRepo) (V.singleton maybeHomepageQR)

      releaseDateTime <- v .: "created_at"

      pure $
        RubyGemsPackage $
          DependencyRegistryInfo
            RubyGems
            maybeRepo
            RASAlive
            (Just releaseDateTime)

parserRubyGemsRepo :: MParser Repo
parserRubyGemsRepo =
  parserCustomQRRepo parserRubyGemsQR

-- ignores the `tree/v6.0.3.1` bit at the end
parserRubyGemsQR :: MParser QualifiedRepo
parserRubyGemsQR =
  parserQualifiedRepo <* M.manyTill M.anySingle M.eof
