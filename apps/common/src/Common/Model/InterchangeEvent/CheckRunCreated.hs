module Common.Model.InterchangeEvent.CheckRunCreated
  ( CheckRunCreated (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Ecosystem.ProgrammingLanguage
import Data.Aeson
import Data.Vector qualified as V

newtype CheckRunCreated = CheckRunCreated
  { _repoProgrammingLanguages :: V.Vector ProgrammingLanguage
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunCreated where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON CheckRunCreated where
  parseJSON = genericParseJSON cleanJSONOptions
