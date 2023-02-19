{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.InstallationAuth.InstallationAuth
  ( InstallationAuth (..),
    obtainInstallationAuth,
    existingInstallationAuth,
  )
where

import Common.Model.GitHub.Auth.GHInstallationAuth
import Common.Model.GitHub.GHAppInstallationId
import Control.Effect.TH

data InstallationAuth (m :: Type -> Type) k where
  ObtainInstallationAuth :: GHAppInstallationId -> InstallationAuth m GHInstallationAuth
  ExistingInstallationAuth :: InstallationAuth m GHInstallationAuth

makeSmartConstructors ''InstallationAuth
