module Common.Model.Details.ComponentTrace
  ( ComponentTrace (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data ComponentTrace = ComponentTrace
  { _spanId :: Text,
    _traceId :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ComponentTrace where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON ComponentTrace where
  parseJSON = genericParseJSON cleanJSONOptions
