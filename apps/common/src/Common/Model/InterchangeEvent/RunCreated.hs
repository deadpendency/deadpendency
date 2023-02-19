module Common.Model.InterchangeEvent.RunCreated
  ( RunCreated (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Details.Run
import Data.Aeson

newtype RunCreated = RunCreated
  { _run :: Run
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RunCreated where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RunCreated where
  parseJSON = genericParseJSON cleanJSONOptions
