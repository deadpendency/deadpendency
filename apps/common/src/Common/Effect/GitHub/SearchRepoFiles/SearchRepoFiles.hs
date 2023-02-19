{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.SearchRepoFiles.SearchRepoFiles
  ( SearchRepoFiles (..),
    repoFilesSearch,
  )
where

import Common.Effect.GitHub.SearchRepoFiles.Model.SearchRepoFilesRequest
import Common.Effect.GitHub.SearchRepoFiles.Model.SearchRepoFilesResult
import Control.Effect.TH

data SearchRepoFiles (m :: Type -> Type) k where
  RepoFilesSearch :: SearchRepoFilesRequest -> SearchRepoFiles m SearchRepoFilesResult

makeSmartConstructors ''SearchRepoFiles
