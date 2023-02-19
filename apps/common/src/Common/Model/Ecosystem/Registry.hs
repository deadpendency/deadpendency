{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Ecosystem.Registry
  ( Registry (..),
    registryAsText,
    -- textToRegistry,
    registryParser,
  )
where

import Common.Aeson.Aeson
import Common.Parsing.Megaparsec (MParser)
import Data.Aeson
import Text.Megaparsec.Char qualified as M

data Registry
  = Npm
  | Hackage
  | Packagist
  | Pypi
  | RubyGems
  | Crates
  | NuGet
  | Maven
  | PkgGoDev
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving anyclass (NFData)

instance ToJSON Registry where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON Registry where
  parseJSON = genericParseJSON cleanJSONOptions

registryAsText :: Registry -> Text
registryAsText =
  \case
    Npm -> "npm"
    Hackage -> "Hackage"
    Packagist -> "Packagist"
    Pypi -> "PyPI"
    RubyGems -> "RubyGems"
    Crates -> "crates.io"
    NuGet -> "NuGet"
    PkgGoDev -> "pkg.go.dev"
    Maven -> "Maven"

-- textToRegistry :: Text -> Maybe Registry
-- textToRegistry =
--   \case
--     "npm" -> Just Npm
--     "Hackage" -> Just Hackage
--     "Packagist" -> Just Packagist
--     "PyPI" -> Just Pypi
--     "RubyGems" -> Just RubyGems
--     "crates.io" -> Just Crates
--     "NuGet" -> Just NuGet
--     "pkg.go.dev" -> Just PkgGoDev
--     "Maven" -> Just Maven
--     _ -> Nothing

registryParser :: MParser Registry
registryParser =
  do
    M.string "npm" $> Npm
    <|> M.string "Hackage" $> Hackage
    <|> M.string "Packagist" $> Packagist
    <|> M.string "PyPI" $> Pypi
    <|> M.string "RubyGems" $> RubyGems
    <|> M.string "crates.io" $> Crates
    <|> M.string "NuGet" $> NuGet
    <|> M.string "pkg.go.dev" $> PkgGoDev
    <|> M.string "Maven" $> Maven
