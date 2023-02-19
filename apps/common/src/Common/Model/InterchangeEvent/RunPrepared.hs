module Common.Model.InterchangeEvent.RunPrepared
  ( RunPrepared (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson
import Data.Vector qualified as V

newtype RunPrepared = RunPrepared
  { _repoProgrammingLanguages :: V.Vector ProgrammingLanguage
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RunPrepared where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON RunPrepared where
  parseJSON = genericParseJSON cleanJSONOptions
