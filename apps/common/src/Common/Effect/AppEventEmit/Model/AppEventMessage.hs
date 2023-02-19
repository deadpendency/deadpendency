module Common.Effect.AppEventEmit.Model.AppEventMessage
  ( AppEventMessage (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

newtype AppEventMessage = AppEventMessage
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AppEventMessage where
  toJSON = genericToJSON cleanJSONOptions
