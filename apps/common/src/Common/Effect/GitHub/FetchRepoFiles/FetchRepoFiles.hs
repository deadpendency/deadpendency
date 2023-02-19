{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.FetchRepoFiles.FetchRepoFiles
  ( FetchRepoFiles (..),
    repoFilesFetch,
  )
where

import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesRequest
import Common.Effect.GitHub.FetchRepoFiles.Model.RepoFilesResult
import Control.Effect.TH

data FetchRepoFiles (m :: Type -> Type) k where
  RepoFilesFetch :: RepoFilesRequest -> FetchRepoFiles m RepoFilesResult

makeSmartConstructors ''FetchRepoFiles
