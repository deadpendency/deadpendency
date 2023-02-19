module Common.Model.Assessment.DependencyAssessmentViolation
  ( DependencyAssessmentViolation (..),
    DAVDeprecationType (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.Registry
import Data.Aeson
import Data.Vector qualified as V

data DependencyAssessmentViolation
  = DAVNoRecentCommits Int (Maybe UTCTime)
  | DAVFewYearlyCommits Int Int
  | DAVNoRecentPackageRelease Int UTCTime
  | DAVSingleRecentAuthor
  | DAVPackageDeprecated Registry DAVDeprecationType (Maybe Text) (V.Vector DependencyName)
  | DAVIsFork
  | DAVRepoArchived
  | DAVRepoNotIdentified
  | DAVRepoNotFound
  deriving stock (Eq, Show, Generic)

data DAVDeprecationType
  = DAVDTDeprecated
  | DAVDTAbandoned
  | DAVDTRelocated
  deriving stock (Eq, Show, Generic, Enum, Bounded)

instance ToJSON DAVDeprecationType where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DAVDeprecationType where
  parseJSON = genericParseJSON cleanJSONOptions

instance ToJSON DependencyAssessmentViolation where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON DependencyAssessmentViolation where
  parseJSON = genericParseJSON cleanJSONOptions
