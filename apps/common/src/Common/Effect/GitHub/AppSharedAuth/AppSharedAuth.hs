{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.AppSharedAuth.AppSharedAuth
  ( AppSharedAuth (..),
    obtainAppSharedAuth,
  )
where

import Common.Model.GitHub.Auth.GHSharedAppAuth
import Control.Effect.TH

data AppSharedAuth (m :: Type -> Type) k where
  ObtainAppSharedAuth :: AppSharedAuth m GHSharedAppAuth

makeSmartConstructors ''AppSharedAuth
