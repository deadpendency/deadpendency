{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Aeson.Aeson
  ( cleanJSONOptions,
    (.:~),
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.List (lookup)
import Data.Vector.NonEmpty qualified as NV
import Text.URI (URI)
import Text.URI qualified as URI

cleanJSONOptions :: Options
cleanJSONOptions =
  defaultOptions
    { fieldLabelModifier = removeLeadingUnderscore,
      unwrapUnaryRecords = True
    }

removeLeadingUnderscore :: String -> String
removeLeadingUnderscore ('_' : xs) = xs
removeLeadingUnderscore xs = xs

-- copied from https://hackage.haskell.org/package/aeson-filthy-0.1.4/docs/Data-Aeson-Filthy.html#v:.:-126-
-- package does not build with latest time
(.:~) :: (FromJSON a) => Object -> Text -> Parser a
o .:~ key = o .: KM.fromText key <|> maybe empty parseJSON go
  where
    go = lookup (KM.fromText (toLower key)) [(KM.fromText $ toLower $ KM.toText k, v) | (k, v) <- KM.toList o]

instance (FromJSON a) => FromJSON (NV.NonEmptyVector a) where
  parseJSON (Array a) =
    case NV.fromVector a of
      Just nev -> NV.mapM parseJSON nev
      Nothing -> fail "Vector must not be empty"
  parseJSON invalid =
    prependFailure
      "parsing NonEmptyVector failed, "
      (typeMismatch "Array" invalid)

instance (ToJSON a) => ToJSON (NV.NonEmptyVector a) where
  toJSON = toJSON . NV.toVector
  toEncoding = toEncoding . NV.toVector

instance ToJSON URI where
  toJSON = toJSON . URI.render
  toEncoding = toEncoding . URI.render

instance FromJSON URI where
  parseJSON = withText "URI" $
    \s -> case URI.mkURI s of
      Left e -> fail $ "URI parse failure: " <> show e
      Right uri -> pure uri
