module Common.Model.RepoConfig.FileLoadPlan
  ( FileLoadPlan (..),
    defaultFileLoadPlan,
  )
where

import Common.Aeson.Aeson
import Common.Model.Ecosystem.ProgrammingLanguage (ProgrammingLanguage)
import Data.Aeson
import Data.Vector.NonEmpty qualified as NV

data FileLoadPlan
  = FileLoadEnabled
  | FileLoadDisabled
  | FileLoadDisabledForLangs (NV.NonEmptyVector ProgrammingLanguage)
  deriving stock (Eq, Show, Generic)

defaultFileLoadPlan :: FileLoadPlan
defaultFileLoadPlan = FileLoadEnabled

instance ToJSON FileLoadPlan where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON FileLoadPlan where
  parseJSON = genericParseJSON cleanJSONOptions
