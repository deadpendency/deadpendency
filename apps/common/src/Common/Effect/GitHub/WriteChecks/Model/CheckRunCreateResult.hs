module Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
  ( CheckRunCreateResult (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.GitHub.Checks.CheckRun
import Data.Aeson
import Data.Vector qualified as V

data CheckRunCreateResult = CheckRunCreateResult
  { _checkRun :: CheckRun,
    _repoProgrammingLanguages :: V.Vector ProgrammingLanguage
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CheckRunCreateResult where
  toJSON = genericToJSON cleanJSONOptions
