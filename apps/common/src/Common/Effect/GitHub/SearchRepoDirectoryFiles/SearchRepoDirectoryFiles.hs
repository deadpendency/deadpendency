{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.SearchRepoDirectoryFiles.SearchRepoDirectoryFiles
  ( SearchRepoDirectoryFiles (..),
    repoDirectoryFilesSearch,
  )
where

import Common.Effect.GitHub.SearchRepoDirectoryFiles.Model.SearchRepoDirectoryFilesRequest
import Common.Effect.GitHub.SearchRepoDirectoryFiles.Model.SearchRepoDirectoryFilesResult
import Control.Effect.TH

data SearchRepoDirectoryFiles (m :: Type -> Type) k where
  RepoDirectoryFilesSearch :: SearchRepoDirectoryFilesRequest -> SearchRepoDirectoryFiles m SearchRepoDirectoryFilesResult

makeSmartConstructors ''SearchRepoDirectoryFiles
