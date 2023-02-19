module Common.Effect.AppEventEmit.Model.AppEventPayload
  ( AppEventPayload (..),
  )
where

import Common.Effect.AppEventEmit.Model.AppEventAdditional
import Common.Effect.AppEventEmit.Model.AppEventMessage
import Common.Model.Details.ComponentDetails
import Common.Model.Details.Run
import Data.Aeson

data AppEventPayload = AppEventPayload
  { _appEventMessage :: AppEventMessage,
    _componentDetails :: ComponentDetails,
    _run :: Maybe Run,
    _appEventAdditional :: Maybe AppEventAdditional
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AppEventPayload where
  toJSON (AppEventPayload message componentDetails maybeRun maybeAdditional) =
    object
      [ "message" .= message,
        "componentDetails" .= componentDetails,
        "run" .= maybeRun,
        "additional" .= maybeAdditional
      ]
