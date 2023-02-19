{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Git.Repo
  ( Repo (..),
  )
where

import Common.Aeson.Aeson
import Common.Model.Git.QualifiedRepo
import Data.Aeson
import Text.URI (URI)

data Repo
  = RepoQR QualifiedRepo
  | RepoUnknown URI
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Ord Repo where
  compare (RepoQR qr) (RepoQR qr') = compare qr qr'
  compare (RepoUnknown repo) (RepoUnknown repo') = compare repo repo'
  compare (RepoQR _) (RepoUnknown _) = GT
  compare (RepoUnknown _) (RepoQR _) = LT

instance ToJSON Repo where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON Repo where
  parseJSON = genericParseJSON cleanJSONOptions

-- getRepoKey :: Repo -> Text
-- getRepoKey (Repo host (RepoOwner owner) (RepoName name)) =
--   let hostKey =
--         case host of
--           GitHub -> "github"
--           Bitbucket -> "bitbucket"
--           GitLab -> "gitlab"
--    in "repo-" <> hostKey <> "-" <> owner <> "-" <> name
