module RG.Loader.ComponentDetailsLoader
  ( loadComponentDetails,
  )
where

import Common.Model.Details.Component
import Common.Model.Details.ComponentDetails

loadComponentDetails :: ComponentDetails
loadComponentDetails =
  ComponentDetails
    { _component = ReportGenerator,
      _componentTextName = "report-generator"
    }
