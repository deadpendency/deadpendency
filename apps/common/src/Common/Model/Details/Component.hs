module Common.Model.Details.Component
  ( Component (..),
  )
where

import Common.Aeson.Aeson
import Data.Aeson

data Component
  = FrontDoor
  | CheckRunCreator
  | RunPreparer
  | DependencyDeterminer
  | DependencyFetcher
  | ReportGenerator
  | RunFinalizer
  | ScriptRunner
  | ErrorProcessor
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON Component where
  toJSON = genericToJSON cleanJSONOptions
