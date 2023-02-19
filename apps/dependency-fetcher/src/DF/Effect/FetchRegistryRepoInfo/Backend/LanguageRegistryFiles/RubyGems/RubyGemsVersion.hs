module DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.RubyGems.RubyGemsVersion
  ( RubyGemsVersion (..),
  )
where

import Data.Aeson

newtype RubyGemsVersion = RubyGemsVersion
  { _version :: Text
  }
  deriving stock (Show)

{-
{
  "version": "6.0.3.1"
}
-}

instance FromJSON RubyGemsVersion where
  parseJSON =
    withObject "RubyGemsVersion" $ \v -> do
      version <- v .: "version"
      pure $ RubyGemsVersion version
