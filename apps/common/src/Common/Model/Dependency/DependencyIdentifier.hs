{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Dependency.DependencyIdentifier
  ( DependencyIdentifier (..),
    getDIRepo,
    getDIKey,
    getDIName,
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyName
import Common.Model.Git.QualifiedRepo
import Data.Aeson

data DependencyIdentifier
  = DependencyIdentifierNamed DependencyName
  | DependencyIdentifierRepo QualifiedRepo (Maybe DependencyName)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- sort ascendingly by dep name. Things without a dep name should appear at the end.
instance Ord DependencyIdentifier where
  compare (DependencyIdentifierRepo qr (Just dn)) (DependencyIdentifierRepo qr' (Just dn')) = compare dn dn' <> compare qr qr'
  compare (DependencyIdentifierRepo qr Nothing) (DependencyIdentifierRepo qr' Nothing) = compare qr qr'
  compare a b =
    case (getDIName a, getDIName b) of
      (Just a', Just b') -> compare a' b'
      (Just _, Nothing) -> LT
      (Nothing, _) -> GT

instance ToJSON DependencyIdentifier where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyIdentifier where
  parseJSON = genericParseJSON cleanJSONOptions

getDIName :: DependencyIdentifier -> Maybe DependencyName
getDIName (DependencyIdentifierNamed depName) = Just depName
getDIName (DependencyIdentifierRepo _ (Just depName)) = Just depName
getDIName _ = Nothing

getDIRepo :: DependencyIdentifier -> Maybe QualifiedRepo
getDIRepo (DependencyIdentifierRepo repo _) = Just repo
getDIRepo _ = Nothing

getDIKey :: DependencyIdentifier -> Text
getDIKey (DependencyIdentifierNamed (DependencyName name)) = name
getDIKey (DependencyIdentifierRepo qr (Just (DependencyName name))) = name <> "-" <> getQRKey qr
getDIKey (DependencyIdentifierRepo qr _) = getQRKey qr
