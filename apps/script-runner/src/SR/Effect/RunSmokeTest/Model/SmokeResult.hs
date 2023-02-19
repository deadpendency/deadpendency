module SR.Effect.RunSmokeTest.Model.SmokeResult (SmokeResult (..), DepsMismatch (..), ErrorsMismatch (..)) where

import Common.Aeson.Aeson
import Common.Model.Git.QualifiedRepo
import Data.Aeson

data SmokeResult
  = SRException QualifiedRepo Text
  | SRFailure QualifiedRepo (These DepsMismatch ErrorsMismatch)
  | SRSuccess
  deriving stock (Eq, Show, Generic)

newtype DepsMismatch = DepsMismatch
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

newtype ErrorsMismatch = ErrorsMismatch
  { _ntText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DepsMismatch where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance ToJSON ErrorsMismatch where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance ToJSON SmokeResult where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions
