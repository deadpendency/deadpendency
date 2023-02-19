module EP.Model.AlwaysJSON
  ( AlwaysJSON (..),
  )
where

import Data.Aeson

-- A way to just ignore when decoding / encoding, as the interchange event details are not important to the failure report
data AlwaysJSON = AlwaysJSON

instance ToJSON AlwaysJSON where
  toJSON _ = Null
  toEncoding _ = toEncoding Null

instance FromJSON AlwaysJSON where
  parseJSON _ = pure AlwaysJSON
