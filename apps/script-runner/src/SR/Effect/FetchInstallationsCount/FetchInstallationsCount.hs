{-# LANGUAGE TemplateHaskell #-}

module SR.Effect.FetchInstallationsCount.FetchInstallationsCount
  ( FetchInstallationsCount (..),
    installationsCountFetch,
  )
where

import Common.Model.Plan.Plan
import Control.Effect.TH

data FetchInstallationsCount (m :: Type -> Type) k where
  InstallationsCountFetch :: Plan -> FetchInstallationsCount m Int

makeSmartConstructors ''FetchInstallationsCount
