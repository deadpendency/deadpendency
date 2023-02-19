module Common.Model.Details.ComponentDetails
  ( ComponentDetails (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Details.Component
import Data.Aeson

data ComponentDetails = ComponentDetails
  { _component :: Component,
    _componentTextName :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ComponentDetails where
  toJSON = genericToJSON cleanJSONOptions
