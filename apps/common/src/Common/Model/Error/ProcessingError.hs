module Common.Model.Error.ProcessingError
  ( ProcessingError (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Error.UserError
import Data.Aeson

data ProcessingError
  = ProcessingErrorApplication
  | ProcessingErrorUser UserError
  deriving stock (Eq, Show, Generic)

instance ToJSON ProcessingError where
  toJSON = genericToJSON cleanJSONOptions

instance FromJSON ProcessingError where
  parseJSON = genericParseJSON cleanJSONOptions
