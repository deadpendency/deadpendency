{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Git.QualifiedRepo
  ( QualifiedRepo (..),
    getQRKey,
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.RepoHost
import Common.Model.Git.RepoName
import Common.Model.Git.RepoOwner
import Data.Aeson

data QualifiedRepo = QualifiedRepo
  { _repoHost :: RepoHost,
    _repoOwner :: RepoOwner,
    _repoName :: RepoName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Ord QualifiedRepo where
  compare (QualifiedRepo _ owner name) (QualifiedRepo _ owner' name') =
    compare owner owner' <> compare name name'

instance ToJSON QualifiedRepo where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON QualifiedRepo where
  parseJSON = genericParseJSON cleanJSONOptions

getQRKey :: QualifiedRepo -> Text
getQRKey (QualifiedRepo host (RepoOwner owner) (RepoName name)) =
  let hostKey =
        case host of
          GitHub -> "github"
          Bitbucket -> "bitbucket"
          GitLab -> "gitlab"
   in "repo-" <> hostKey <> "-" <> owner <> "-" <> name
