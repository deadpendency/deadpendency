{-# LANGUAGE DataKinds #-}

module FD.Model.LoadedGitHubKey (LoadedGitHubKey (..)) where

import Servant (Context (..), HasContextEntry, getContextEntry)
import Servant.GitHub.Webhook qualified as SGW

newtype LoadedGitHubKey
  = LoadedGitHubKey (forall result. SGW.GitHubKey result)

instance HasContextEntry '[LoadedGitHubKey] (SGW.GitHubKey result) where
  getContextEntry (LoadedGitHubKey x :. _) = x
