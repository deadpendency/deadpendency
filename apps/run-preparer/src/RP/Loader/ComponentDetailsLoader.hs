module RP.Loader.ComponentDetailsLoader
  ( loadComponentDetails,
  )
where

import Common.Model.Details.Component
import Common.Model.Details.ComponentDetails

loadComponentDetails :: ComponentDetails
loadComponentDetails =
  ComponentDetails
    { _component = RunPreparer,
      _componentTextName = "run-preparer"
    }
